// See LICENSE for license details.

package chisel3.tester

import chisel3._
import chisel3.util._

// implicit class, cannot maintain state
class DecoupledDriver[T <: Data](x: DecoupledIO[T]) {
  // Source (enqueue) functions
  //
  def initSource(): Unit = {
    x.valid.poke(false.B)
  }

  def setSourceClock(clock: Clock) = {
    ClockResolutionUtils.setClock(DecoupledDriver.decoupledSourceKey, x, clock)
  }

  protected def getSourceClock: Clock = {
    ClockResolutionUtils.getClock(DecoupledDriver.decoupledSourceKey, x,
      x.ready.getSourceClock)  // TODO: validate against bits/valid sink clocks
  }

  def enqueueNow(data: T): Unit = timescope {
    // TODO: check for init
    x.bits.poke(data)
    x.valid.poke(true.B)
    x.ready.expect(true.B)
    getSourceClock.step(1)
  }

  def enqueue(data: T): Unit = timescope {
    // TODO: check for init
    x.bits.poke(data)
    x.valid.poke(true.B)
    fork.withRegion(Monitor) {
      while (x.ready.peek().litToBoolean == false) {
        getSourceClock.step(1)
      }
    }.joinAndStep(getSourceClock)
  }

  def enqueueSeq(data: Seq[T]): Unit = timescope {
    for (elt <- data) {
      enqueue(elt)
    }
  }

  // Sink (dequeue) functions
  //
  def initSink(): Unit = {
    x.ready.poke(false.B)
  }

  def setSinkClock(clock: Clock) = {
    ClockResolutionUtils.setClock(DecoupledDriver.decoupledSinkKey, x, clock)
  }

  protected def getSinkClock: Clock = {
    ClockResolutionUtils.getClock(DecoupledDriver.decoupledSinkKey, x,
      x.valid.getSourceClock)  // TODO: validate against bits/valid sink clocks
  }

  def waitForValid(): Unit = {
    while (x.valid.peek().litToBoolean == false) {
      getSinkClock.step(1)
    }
  }

  def expectDequeue(data: T): Unit = timescope {
    // TODO: check for init
    x.ready.poke(true.B)
    fork.withRegion(Monitor) {
      waitForValid()
      x.valid.expect(true.B)
      x.bits.expect(data)
    }.joinAndStep(getSinkClock)
  }

  def expectDequeueNow(data: T): Unit = timescope {
    // TODO: check for init
    x.ready.poke(true.B)
    fork.withRegion(Monitor) {
      x.valid.expect(true.B)
      x.bits.expect(data)
    }.joinAndStep(getSinkClock)
  }

  def expectDequeueSeq(data: Seq[T]): Unit = timescope {
    for (elt <- data) {
      expectDequeue(elt)
    }
  }

  def expectPeek(data: T): Unit = {
    x.valid.expect(true.B)
    x.bits.expect(data)
  }

  def expectInvalid(): Unit = {
    x.valid.expect(false.B)
  }
}

object DecoupledDriver {
  protected val decoupledSourceKey = new Object()
  protected val decoupledSinkKey = new Object()
}