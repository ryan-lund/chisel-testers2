package chiseltest.legacy.backends.verilator

import java.io.{BufferedReader, File, FileReader, FileWriter}

import chiseltest.backends.BackendExecutive
import chiseltest.internal._
import chisel3.{MultiIOModule, assert}
import chisel3.experimental.DataMirror
import chisel3.stage.{ChiselCircuitAnnotation, ChiselStage}
import firrtl.annotations.ReferenceTarget
import firrtl.stage.CompilerAnnotation
import firrtl.transforms.CombinationalPath

import scala.sys.process._
import com.github.plokhotnyuk.jsoniter_scala.macros._
import com.github.plokhotnyuk.jsoniter_scala.core._

object VerilatorExecutive extends BackendExecutive {
  import firrtl._

  /** Verilator wants to have module name prefix except for
    * default reset and clock
    *
    * @param component signal name to be mapped into backend string form
    * @return
    */
  def componentToName(component: ReferenceTarget): String = {
    component.name match {
      case "reset" => "reset"
      case "clock" => "clock"
      case _ =>
        s"${component.module}.${component.name}"
    }
  }

  def start[T <: MultiIOModule](
    dutGen: () => T,
    annotationSeq: AnnotationSeq
  ): BackendInstance[T] = {

    // Force a cleanup: long SBT runs tend to fail with memory issues
    System.gc()

    val targetDir = annotationSeq.collectFirst {
      case TargetDirAnnotation(t) => t
    }.get
    val targetDirFile = new File(targetDir)

    val generatorAnnotation = chisel3.stage.ChiselGeneratorAnnotation(dutGen)
    val elaboratedAnno = (new chisel3.stage.phases.Elaborate).transform(annotationSeq :+ generatorAnnotation)
    val circuit = elaboratedAnno.collect { case x: ChiselCircuitAnnotation => x }.head.circuit
    val dut = getTopModule(circuit).asInstanceOf[T]

    if (elaboratedAnno.contains(CachingAnnotation)) {
      // Generate new high FIRRTL for caching
      val highFirrtlPath = s"${targetDir}/${circuit.name}.hi.fir"
      val oldHighFirrtlPath = s"${targetDir}/${circuit.name}.hi.fir.old"
      s"mv ${highFirrtlPath} ${oldHighFirrtlPath}".!

      (new ChiselStage).run(
        elaboratedAnno :+ CompilerAnnotation(new HighFirrtlCompiler())
      )

      // Diff high FIRRTL and delete old file
      val diffExit = s"diff ${highFirrtlPath} ${oldHighFirrtlPath}".!
      s"rm -rf ${oldHighFirrtlPath}".!

      // Generate new hash
      val hashFile = new File(targetDir, "elaborationDetails.hash")

      // Hash the deterministic elements of elaboratedAnno for comparison
      // currently since this is guaranteed to be in Verilator and have the same test run dir, this should
      // only catch if coverage or vcd flags change
      val elaboratedAnnoHash = elaboratedAnno
        .toSeq
        .filter(anno => !List("chisel3.stage.ChiselCircuitAnnotation", "chisel3.stage.DesignAnnotation")
          .contains(anno.getClass.toString.split(" ").last))
        .map(anno => anno.hashCode)
      val firrtlInfoHash = Seq(firrtl.BuildInfo.hashCode)
      val systemVersionHash = Seq(System.getProperty("java.version"), scala.util.Properties.versionNumberString)
        .map(version => version.hashCode)

      val hash = (elaboratedAnnoHash ++ firrtlInfoHash ++ systemVersionHash).toString

      val oldHash = if (hashFile.exists) { new BufferedReader(new FileReader(hashFile)).readLine } else { " " }
      val elaboratedAnnoWriter = new FileWriter(hashFile)
      elaboratedAnnoWriter.write(hash)
      elaboratedAnnoWriter.close()

      if (diffExit == 0 && hash == oldHash) {
        println("--- Hash and high FIRRTL matched: Using cached data ---")
        val portNames = DataMirror
          .modulePorts(dut)
          .flatMap {
            case (name, data) =>
              getDataNames(name, data).toList.map {
                case (p, "reset") => (p, "reset")
                case (p, "clock") => (p, "clock")
                case (p, n) => (p, s"${circuit.name}.$n")
                //          case (p, n) => (p, s"$n")
              }
          }
          .toMap

        val pathsJson = new BufferedReader(new FileReader(new File(targetDir, "paths.json"))).readLine()
        val pathsCodec: JsonValueCodec[Seq[CombinationalPath]] = JsonCodecMaker.make
        val paths = readFromArray(pathsJson.getBytes("UTF-8"))(pathsCodec)

        val pathsAsData =
          combinationalPathsToData(dut, paths, portNames, componentToName)

        val commandJson = new BufferedReader(new FileReader(new File(targetDir, "command.json"))).readLine()
        val commandCodec: JsonValueCodec[Seq[String]] = JsonCodecMaker.make
        val command = readFromArray(commandJson.getBytes("UTF-8"))(commandCodec)

        return new VerilatorBackend(dut, portNames, pathsAsData, command)
      }
    }

    println("--- Building from scratch ---")

    // Create the header files that verilator needs
    CopyVerilatorHeaderFiles(targetDir)

    // Generate the verilog file and some or all of the following annotations
    // - OutputFileAnnotation
    // - VerilatorFlagsAnnotation
    // - VerilatorCFlagsAnnotation
    // - CommandEditsFile
    // - TestCommandOverride
    // - CombinationalPath
    val compiledAnnotations = (new ChiselStage).run(
      elaboratedAnno :+ CompilerAnnotation(new VerilogCompiler())
    )

    val cppHarnessFileName = s"${circuit.name}-harness.cpp"
    val cppHarnessFile = new File(targetDir, cppHarnessFileName)
    val cppHarnessWriter = new FileWriter(cppHarnessFile)
    val vcdFile = new File(targetDir, s"${circuit.name}.vcd")
    val emittedStuff =
      VerilatorCppHarnessGenerator.codeGen(dut, vcdFile.toString, targetDir)
    cppHarnessWriter.append(emittedStuff)
    cppHarnessWriter.close()

    val moreVerilatorFlags = compiledAnnotations
      .collectFirst { case VerilatorFlags(f) => f }
      .getOrElse(Seq.empty)
    val moreVerilatorCFlags = compiledAnnotations
      .collectFirst { case VerilatorCFlags(f) => f }
      .getOrElse(Seq.empty)
    val writeVcdFlag = if(compiledAnnotations.contains(WriteVcdAnnotation)) { Seq("--trace") } else { Seq() }
    val coverageFlags = Seq((compiledAnnotations collect {
      case LineCoverageAnnotation => List("--coverage-line")
      case ToggleCoverageAnnotation => List("--coverage-toggle")
      case UserCoverageAnnotation => List("--coverage-user")
      case StructuralCoverageAnnotation => List("--coverage-line", "--coverage-toggle")
      }).flatten.distinct.mkString(" ")
    )

    val commandEditsFile = compiledAnnotations
      .collectFirst { case CommandEditsFile(f) => f }
      .getOrElse("")

    val coverageFlag = if(compiledAnnotations.intersect(Seq(LineCoverageAnnotation, ToggleCoverageAnnotation, UserCoverageAnnotation)).nonEmpty) {
      Seq("-DSP_COVERAGE_ENABLE") } else { Seq ()
    }

    val verilatorFlags = moreVerilatorFlags ++ writeVcdFlag ++ coverageFlags
    val verilatorCFlags = moreVerilatorCFlags ++ coverageFlag

    assert(
      verilogToVerilator(
        circuit.name,
        new File(targetDir),
        cppHarnessFile,
        moreVerilatorFlags = verilatorFlags,
        moreVerilatorCFlags = verilatorCFlags,
        editCommands = commandEditsFile
      ).! == 0,
      s"verilator command failed on circuit ${circuit.name} in work dir $targetDir"
    )
    assert(
      chisel3.Driver.cppToExe(circuit.name, targetDirFile).! == 0,
      s"Compilation of verilator generated code failed for circuit ${circuit.name} in work dir $targetDir"
    )

    val command = compiledAnnotations
      .collectFirst[Seq[String]] {
        case TestCommandOverride(f) => f.split(" +")
      }
      .getOrElse { Seq(new File(targetDir, s"V${circuit.name}").toString) }

    val portNames = DataMirror
      .modulePorts(dut)
      .flatMap {
        case (name, data) =>
          getDataNames(name, data).toList.map {
            case (p, "reset") => (p, "reset")
            case (p, "clock") => (p, "clock")
            case (p, n)       => (p, s"${circuit.name}.$n")
            //          case (p, n) => (p, s"$n")
          }
      }
      .toMap

    val paths = compiledAnnotations.collect { case c: CombinationalPath => c }

    if (elaboratedAnno.contains(CachingAnnotation)) {
      // Cache paths as json as they depend on the compiledAnnotations
      val pathsCodec: JsonValueCodec[Seq[CombinationalPath]] = JsonCodecMaker.make
      val pathsWriter = new FileWriter(new File(targetDir, "paths.json"))
      pathsWriter.write(new String(writeToArray(paths)(pathsCodec), "UTF-8"))
      pathsWriter.close()

      // Cache command
      val commandCodec: JsonValueCodec[Seq[String]] = JsonCodecMaker.make
      val commandWriter = new FileWriter(new File(targetDir, "command.json"))
      commandWriter.write(new String(writeToArray(command)(commandCodec), "UTF-8"))
      commandWriter.close()
    }

    val pathsAsData =
      combinationalPathsToData(dut, paths, portNames, componentToName)

    new VerilatorBackend(dut, portNames, pathsAsData, command)
  }
}
