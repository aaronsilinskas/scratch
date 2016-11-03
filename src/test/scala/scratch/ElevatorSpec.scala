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
      val pickupFloor = Random.nextInt(10)
      val elevatorFloor = Random.nextInt(10)

      val elevator = new Elevator(Available(elevatorFloor))
      val scheduler = new Scheduler(elevator)

      When("the pickup is requested")
      scheduler.requestPickup(pickupFloor)

      Then("the elevator moves to the pickup floor, opens the door, and closes it")
      elevator.history shouldBe Seq(
        Available(elevatorFloor),
        Move(pickupFloor),
        Stopped(pickupFloor),
        Opening,
        Open,
        Wait(5, duration.SECONDS),
        Closing,
        Closed
      )
    }

    scenario("A drop-off request for an available elevator on different floors") {
      Given("a drop-off request and an available elevator")
      val dropOffFloor = Random.nextInt(10)
      val elevatorFloor = Random.nextInt(10)

      val elevator = new Elevator(Available(elevatorFloor))
      val scheduler = new Scheduler(elevator)

      When("the drop-off is requested")
      scheduler.requestDropOff(dropOffFloor)

      Then("the elevator moves to the drop-off floor, opens the door, and closes it")
      elevator.history shouldBe Seq(
        Available(elevatorFloor),
        Move(dropOffFloor),
        Stopped(dropOffFloor),
        Opening,
        Open,
        Wait(5, duration.SECONDS),
        Closing,
        Closed
      )
    }

    scenario("A pickup request between an elevator and scheduled drop-offs going in the same direction") {
      Given("an elevator with scheduled drop-offs")

      And("a pickup request between the elevator and drop-offs going in the same direction")

      When("the pickup is requested")

      Then("the elevator finishes drop-offs before the pick-up floor")

      And("the elevator opens the door and closes it on the pickup floor")

      And("the elevator finishes the drop-offs after the pick-up floor")
      pending
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

  private def moveToOpenAndClose(floor: Int): Unit = {
    _history ++= Seq[ElevatorState](
      Move(floor),
      Stopped(floor),
      Opening,
      Open,
      Wait(5, duration.SECONDS),
      Closing,
      Closed
    )
  }

  def history(): Seq[ElevatorState] = {
    _history
  }
}

class Scheduler(elevator: Elevator) {

  def requestPickup(floor: Int): Unit = {
    elevator.pickup(floor)
  }

  def requestDropOff(floor: Int): Unit = {
    elevator.dropOff(floor)
  }
}


sealed trait ElevatorState

case class Available(floor: Int) extends ElevatorState

case class Move(floor: Int) extends ElevatorState

case class Stopped(floor: Int) extends ElevatorState

object Opening extends ElevatorState

object Open extends ElevatorState

case class Wait(time: Long, unit: TimeUnit) extends ElevatorState

object Closing extends ElevatorState

object Closed extends ElevatorState


