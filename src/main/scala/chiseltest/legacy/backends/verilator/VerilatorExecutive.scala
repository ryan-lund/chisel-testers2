package chiseltest.legacy.backends.verilator

import java.io.{BufferedReader, File, FileReader, FileWriter}

import chiseltest.backends.BackendExecutive
import chiseltest.internal._
import chisel3.{Module, assert}
import chisel3.experimental.DataMirror
import chisel3.stage.{ChiselCircuitAnnotation, ChiselStage}
import firrtl.annotations.ReferenceTarget
import firrtl.ir.StructuralHash
import firrtl.stage.RunFirrtlTransformAnnotation
import firrtl.transforms.CombinationalPath
import firrtl.util.BackendCompilationUtilities
import java.nio.file.{Files, Paths}
import java.security.MessageDigest
import javax.xml.bind.DatatypeConverter

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

  def start[T <: Module](
    dutGen:        () => T,
    annotationSeq: AnnotationSeq
  ): BackendInstance[T] = {

    // Force a cleanup: long SBT runs tend to fail with memory issues
    System.gc()

    var targetDir = annotationSeq.collectFirst {
        case firrtl.options.TargetDirAnnotation(t) => t
      }.get
    var targetDirFile = new File(targetDir)

    val uncachedTargetDir = targetDir

    val generatorAnnotation = chisel3.stage.ChiselGeneratorAnnotation(dutGen)
    val elaboratedAnno = (new chisel3.stage.phases.Elaborate).transform(annotationSeq :+ generatorAnnotation)
    val circuit = elaboratedAnno.collect { case x: ChiselCircuitAnnotation => x }.head.circuit
    val dut = getTopModule(circuit).asInstanceOf[T]

    if (elaboratedAnno.contains(CachingAnnotation)) {
      val cacheDir = s"${targetDirFile.getParent}/cache_dir"

      val highFirrtlAnnos = (new ChiselStage).run(
        elaboratedAnno :+ RunFirrtlTransformAnnotation(new HighFirrtlEmitter)
      )

      val rawFirrtl = highFirrtlAnnos
        .collect { case EmittedFirrtlCircuitAnnotation(e) => e.value}
        .map(l => firrtl.Parser.parse(l))
        .head

      val moduleHashString = DatatypeConverter
        .printHexBinary(MessageDigest
          .getInstance("SHA-256")
          .digest(new firrtl.stage.transforms.Compiler(targets = firrtl.stage.Forms.HighForm)
            .transform(firrtl.CircuitState(rawFirrtl, Seq()))
            .circuit
            .modules
            .map(m => m.name -> StructuralHash.sha256WithSignificantPortNames(m))
            .sortBy(_._1)
            .map(_._2.hashCode().toString) // TODO: Replace .hashCode().toString with .str
            .toString()
            .getBytes("UTF-8")))

      // Hash the deterministic elements of elaboratedAnno for comparison, this should
      // only catch if coverage or vcd flags change
      val elaboratedAnnoString = elaboratedAnno
        .toSeq
        .filter(anno => !List("chisel3.stage.ChiselCircuitAnnotation", "chisel3.stage.DesignAnnotation", "firrtl.options.TargetDirAnnotation")
          .contains(anno.getClass.toString.split(" ").last))
        .map(anno => anno.toString)
      val firrtlInfoString = Seq(firrtl.BuildInfo.toString)
      val systemVersionString = Seq(System.getProperty("java.version"), scala.util.Properties.versionNumberString)

      val elaboratedAnnoHashString = DatatypeConverter
        .printHexBinary(MessageDigest
          .getInstance("SHA-256")
          .digest((elaboratedAnnoString ++ firrtlInfoString ++ systemVersionString)
            .toString
            .getBytes("UTF-8")))

      val annoCacheDir = s"${cacheDir}/${moduleHashString}/${elaboratedAnnoHashString}"

      targetDir = annoCacheDir
      targetDirFile = new File(annoCacheDir)

      if (targetDirFile.exists()) { // Check if we have built a simulator with matching module and annotations
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

        // Symlink location of Verilator and Verilog sources for user reference
        val sourcePath = s"${uncachedTargetDir}/sources"
        new File(sourcePath).delete()
        Files.createSymbolicLink(Paths.get(sourcePath), Paths.get(targetDirFile.getAbsolutePath))

        return new VerilatorBackend(dut, portNames, pathsAsData, command)
      } else {
        targetDirFile.mkdirs()
      }
    }

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
      if (elaboratedAnno.contains(CachingAnnotation)) {
        AnnotationSeq(elaboratedAnno.toSeq
          .filter(anno => !anno.getClass.toString.split(" ").last.equals("firrtl.options.TargetDirAnnotation"))
        :+ firrtl.options.TargetDirAnnotation(targetDir))
      } else {
        elaboratedAnno
      } :+ RunFirrtlTransformAnnotation(new VerilogEmitter)
    )

    val cppHarnessFileName = s"${circuit.name}-harness.cpp"
    val cppHarnessFile = new File(targetDir, cppHarnessFileName)
    val cppHarnessWriter = new FileWriter(cppHarnessFile)
    val vcdFile = new File(uncachedTargetDir, s"${circuit.name}.vcd")
    val coverageDir = s"${uncachedTargetDir}/logs"
    val emittedStuff =
      VerilatorCppHarnessGenerator.codeGen(dut, vcdFile.toString, coverageDir)
    cppHarnessWriter.append(emittedStuff)
    cppHarnessWriter.close()

    val moreVerilatorFlags = compiledAnnotations.collectFirst { case VerilatorFlags(f) => f }
      .getOrElse(Seq.empty)
    val moreVerilatorCFlags = compiledAnnotations.collectFirst { case VerilatorCFlags(f) => f }
      .getOrElse(Seq.empty)
    val writeVcdFlag = if (compiledAnnotations.contains(WriteVcdAnnotation)) { Seq("--trace") }
    else { Seq() }
    val coverageFlags = Seq((compiledAnnotations.collect {
      case LineCoverageAnnotation       => List("--coverage-line")
      case ToggleCoverageAnnotation     => List("--coverage-toggle")
      case UserCoverageAnnotation       => List("--coverage-user")
      case StructuralCoverageAnnotation => List("--coverage-line", "--coverage-toggle")
    }).flatten.distinct.mkString(" "))

    val commandEditsFile = compiledAnnotations.collectFirst { case CommandEditsFile(f) => f }
      .getOrElse("")

    val coverageFlag =
      if (
        compiledAnnotations
          .intersect(Seq(LineCoverageAnnotation, ToggleCoverageAnnotation, UserCoverageAnnotation))
          .nonEmpty
      ) {
        Seq("-DSP_COVERAGE_ENABLE")
      } else { Seq() }

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
      BackendCompilationUtilities.cppToExe(circuit.name, targetDirFile).! == 0,
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

      // Symlink location of Verilator and Verilog sources for user reference
      val sourcePath = s"${uncachedTargetDir}/sources"
      new File(sourcePath).delete()
      Files.createSymbolicLink(Paths.get(s"${uncachedTargetDir}/sources"), Paths.get(targetDirFile.getAbsolutePath))
    }

    val pathsAsData =
      combinationalPathsToData(dut, paths, portNames, componentToName)

    new VerilatorBackend(dut, portNames, pathsAsData, command)
  }
}
