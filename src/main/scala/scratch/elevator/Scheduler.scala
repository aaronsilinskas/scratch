package scratch.elevator

import scala.collection.mutable

class Scheduler(elevator: Elevator) {

  private val scheduled = mutable.Queue[SchedulerRequest]()

  def run() = {
    val elevatorFloor = elevator.floor()

    val (pickups, dropOffs) = scheduled.partition(_.isInstanceOf[PickupRequest])

    val direction = elevatorDirection(elevatorFloor, dropOffs)

    val orderedScheduled = scheduled.sortBy {
      case pr: PickupRequest => Math.abs(pr.floor - elevatorFloor)
      case dr: DropOffRequest => Math.abs(dr.floor - elevatorFloor)
    }

    orderedScheduled foreach {
      case pr: PickupRequest => elevator.pickup(pr.floor)
      case dr: DropOffRequest => elevator.dropOff(dr.floor)
    }
  }

  def requestPickup(floor: Int, direction: ElevatorDirection): Unit = {
    scheduled += PickupRequest(floor, direction)
  }

  def requestDropOff(floor: Int): Unit = {
    scheduled += DropOffRequest(floor)
  }

  private def elevatorDirection(elevatorFloor: Int, destinationFloors: mutable.Queue[SchedulerRequest]):ElevatorDirection = {
    val nextFloor = destinationFloors.front.floor()
    if (elevatorFloor < nextFloor) Up
    else Down
  }
}


sealed trait SchedulerRequest {
  def floor(): Int
}

case class PickupRequest(floor: Int, direction: ElevatorDirection) extends SchedulerRequest

case class DropOffRequest(floor: Int) extends SchedulerRequest
