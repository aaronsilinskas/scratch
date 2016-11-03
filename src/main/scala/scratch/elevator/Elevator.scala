package scratch.elevator

import scala.collection.mutable
import scala.concurrent.duration
import scala.concurrent.duration._

class Elevator(initialState: ElevatorState) {

  private val _history = mutable.Queue[ElevatorState](initialState)

  def pickup(floor: Int): Unit = {
    moveToOpenAndClose(floor)
  }

  def dropOff(floor: Int): Unit = {
    moveToOpenAndClose(floor)
  }

  def floor(): Int = {
    _history.last.floor
  }

  private def moveToOpenAndClose(floor: Int): Unit = {
    _history ++= Seq[ElevatorState](
      Move(floor),
      Stopped(floor),
      Opening(floor),
      Open(floor),
      Wait(floor, 5, duration.SECONDS),
      Closing(floor),
      Closed(floor)
    )
  }

  def history(): Seq[ElevatorState] = {
    _history
  }
}

sealed abstract class ElevatorState {
  def floor: Int
}

case class Available(floor: Int) extends ElevatorState

case class Move(floor: Int) extends ElevatorState

case class Stopped(floor: Int) extends ElevatorState

case class Opening(floor: Int) extends ElevatorState

case class Open(floor: Int) extends ElevatorState

case class Wait(floor: Int, time: Long, unit: TimeUnit) extends ElevatorState

case class Closing(floor: Int) extends ElevatorState

case class Closed(floor: Int) extends ElevatorState

sealed trait ElevatorDirection

object Up extends ElevatorDirection

object Down extends ElevatorDirection