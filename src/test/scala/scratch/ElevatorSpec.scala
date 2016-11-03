package scratch

import org.scalatest.{FeatureSpec, GivenWhenThen, Matchers}

import scala.collection.mutable
import scala.concurrent.duration
import scala.concurrent.duration.TimeUnit
import scala.util.Random

/*
Notes for requirements and states:

Basic Elevator
- open a closing door
- close an open door without waiting

Scheduling
- elevator pickup in same direction as scheduled drop-offs
- elevator pickup in different direction from scheduled drop-offs

Multiple Elevators - Super Simple Scheduling
- closest elevator going in same direction handles pickup
- closest elevator that finishes drop-offs handles pickup

Pluggable Schedulers
- custom scheduling logic can be used

Indicators
- pickup and drop-off updates should be available for UI like button lights

Maintenance
- elevators taken out of service aren't scheduled
- elevators can be manually controlled
- elevators can be put back into service

Extra stuff
- emergency call
- schedulers should take start, stop, transit, and open+close time into account
- secure floors require authentication before drop-off requests are allowed
*/
class ElevatorSpec extends FeatureSpec with GivenWhenThen with Matchers {

  feature("Elevator pickup and drop-off.") {

    scenario("A pickup request for an available elevator on a different floor") {
      Given("a pickup request and available elevator")
      val elevatorFloor = Random.nextInt(10)
      val elevator = new Elevator(Available(elevatorFloor))
      val scheduler = new Scheduler(elevator)

      val pickupFloor = Random.nextInt(10)
      scheduler.requestPickup(pickupFloor)

      When("the elevator runs")
      scheduler.run()

      Then("the elevator moves to the pickup floor, opens the door, and closes it")
      elevator.history shouldBe Seq(
        Available(elevatorFloor),
        Move(pickupFloor),
        Stopped(pickupFloor),
        Opening(pickupFloor),
        Open(pickupFloor),
        Wait(pickupFloor, 5, duration.SECONDS),
        Closing(pickupFloor),
        Closed(pickupFloor)
      )
    }

    scenario("A drop-off request for an available elevator on different floors") {
      Given("a drop-off request and an available elevator")
      val elevatorFloor = Random.nextInt(10)
      val elevator = new Elevator(Available(elevatorFloor))
      val scheduler = new Scheduler(elevator)

      val dropOffFloor = Random.nextInt(10)
      scheduler.requestDropOff(dropOffFloor)

      When("the elevator runs")
      scheduler.run()

      Then("the elevator moves to the drop-off floor, opens the door, and closes it")
      elevator.history shouldBe Seq(
        Available(elevatorFloor),
        Move(dropOffFloor),
        Stopped(dropOffFloor),
        Opening(dropOffFloor),
        Open(dropOffFloor),
        Wait(dropOffFloor, 5, duration.SECONDS),
        Closing(dropOffFloor),
        Closed(dropOffFloor)
      )
    }

    scenario("A pickup request between an elevator and scheduled drop-offs going in the same direction") {
      Given("scheduled drop-offs before and after a pickup request going in the same direction")
      val elevatorDirection = if (Random.nextBoolean()) Up else Down
      val elevatorFloor = 100 + Random.nextInt(10)
      val dropOffBeforePickupFloor = afterFloor(elevatorFloor, elevatorDirection)
      val pickupFloor = afterFloor(dropOffBeforePickupFloor, elevatorDirection)
      val dropOffAfterFloor = afterFloor(pickupFloor, elevatorDirection)

      val elevator = new Elevator(Available(elevatorFloor))
      val scheduler = new Scheduler(elevator)
      scheduler.requestDropOff(dropOffBeforePickupFloor)
      scheduler.requestDropOff(dropOffAfterFloor)
      scheduler.requestPickup(pickupFloor)

      When("the elevators run")
      scheduler.run()

      Then("the elevator finishes drop-offs before the pick-up floor")
      val h = elevator.history()
      h.take(8) shouldBe Seq(
        Available(elevatorFloor),
        Move(dropOffBeforePickupFloor),
        Stopped(dropOffBeforePickupFloor),
        Opening(dropOffBeforePickupFloor),
        Open(dropOffBeforePickupFloor),
        Wait(dropOffBeforePickupFloor, 5, duration.SECONDS),
        Closing(dropOffBeforePickupFloor),
        Closed(dropOffBeforePickupFloor)
      )

      And("the elevator opens the door and closes it on the pickup floor")
      h.slice(8, 15) shouldBe Seq(
        Move(pickupFloor),
        Stopped(pickupFloor),
        Opening(pickupFloor),
        Open(pickupFloor),
        Wait(pickupFloor, 5, duration.SECONDS),
        Closing(pickupFloor),
        Closed(pickupFloor)
      )

      And("the elevator finishes the drop-offs after the pick-up floor")
      h.slice(15, 22) shouldBe Seq(
        Move(dropOffAfterFloor),
        Stopped(dropOffAfterFloor),
        Opening(dropOffAfterFloor),
        Open(dropOffAfterFloor),
        Wait(dropOffAfterFloor, 5, duration.SECONDS),
        Closing(dropOffAfterFloor),
        Closed(dropOffAfterFloor)
      )
    }
  }

  def afterFloor(floor: Int, direction: ElevatorDirection): Int = {
    direction match {
      case Up => floor + 1 + Random.nextInt(10)
      case Down => floor - 1 - Random.nextInt(10)
    }
  }
}

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

class Scheduler(elevator: Elevator) {

  private val scheduled = mutable.Queue[SchedulerRequest]()

  def run() = {
    val elevatorFloor = elevator.floor()

    val orderedScheduled = scheduled.sortBy {
      case pr: PickupRequest => Math.abs(pr.floor - elevatorFloor)
      case dr: DropOffRequest => Math.abs(dr.floor - elevatorFloor)
    }

    orderedScheduled foreach {
      case pr: PickupRequest => elevator.pickup(pr.floor)
      case dr: DropOffRequest => elevator.dropOff(dr.floor)
    }
  }

  def requestPickup(floor: Int): Unit = {
    scheduled += PickupRequest(floor)
  }

  def requestDropOff(floor: Int): Unit = {
    scheduled += DropOffRequest(floor)
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

sealed trait SchedulerRequest

case class PickupRequest(floor: Int) extends SchedulerRequest

case class DropOffRequest(floor: Int) extends SchedulerRequest