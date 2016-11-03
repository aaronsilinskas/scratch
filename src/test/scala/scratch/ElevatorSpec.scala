package scratch

import org.scalatest.{FeatureSpec, GivenWhenThen, Matchers}
import scratch.elevator._

import scala.concurrent.duration
import scala.util.Random

/*
Notes for requirements and states:

Basic Elevator
- open a closing door
- close an open door without waiting

Scheduling
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
      val elevatorFloor = randomElevatorStartFloor()
      val elevator = new Elevator(Available(elevatorFloor))
      val scheduler = new Scheduler(elevator)

      val pickupFloor = afterFloor(elevatorFloor)
      scheduler.requestPickup(pickupFloor)

      When("the elevator runs")
      scheduler.run()

      Then("the elevator moves to the pickup floor, opens the door, and closes it")
      elevator.history shouldBe historySeq(
        available(elevatorFloor),
        moveTo(pickupFloor),
        openAndClose(pickupFloor)
      )
    }

    scenario("A drop-off request for an available elevator on different floors") {
      Given("a drop-off request and an available elevator")
      val elevatorFloor = randomElevatorStartFloor()
      val elevator = new Elevator(Available(elevatorFloor))
      val scheduler = new Scheduler(elevator)

      val dropOffFloor = afterFloor(elevatorFloor)
      scheduler.requestDropOff(dropOffFloor)

      When("the elevator runs")
      scheduler.run()

      Then("the elevator moves to the drop-off floor, opens the door, and closes it")
      elevator.history shouldBe historySeq(
        available(elevatorFloor),
        moveTo(dropOffFloor),
        openAndClose(dropOffFloor)
      )
    }

    scenario("A pickup request between an elevator and scheduled drop-offs going in the same direction") {
      Given("scheduled drop-offs before and after a pickup request going in the same direction")
      val elevatorDirection = randomElevatorDirection()
      val elevatorFloor = randomElevatorStartFloor()
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
      val remaining1 = verifyPartOfHistory(elevator.history(),
        available(elevatorFloor),
        moveTo(dropOffBeforePickupFloor),
        openAndClose(dropOffBeforePickupFloor)
      )

      And("the elevator opens the door and closes it on the pickup floor")
      val remaining2 = verifyPartOfHistory(remaining1,
        moveTo(pickupFloor),
        openAndClose(pickupFloor)
      )

      And("the elevator finishes the drop-offs after the pick-up floor")
      remaining2 shouldBe historySeq(
        moveTo(dropOffAfterFloor),
        openAndClose(dropOffAfterFloor)
      )
    }
  }

  def randomElevatorDirection(): ElevatorDirection = {
    if (Random.nextBoolean()) Up else Down
  }

  def randomElevatorStartFloor(): Int = {
    100 + Random.nextInt(10)
  }

  def afterFloor(floor: Int, direction: ElevatorDirection = randomElevatorDirection()): Int = {
    direction match {
      case Up => floor + 1 + Random.nextInt(10)
      case Down => floor - 1 - Random.nextInt(10)
    }
  }

  def historySeq(transitions: Seq[ElevatorState]*): Seq[ElevatorState] = {
    transitions.flatten
  }

  def available(floor: Int): Seq[ElevatorState] = {
    Seq(Available(floor))
  }

  def moveTo(floor: Int): Seq[ElevatorState] = {
    Seq(
      Move(floor),
      Stopped(floor)
    )
  }

  def openAndClose(floor: Int): Seq[ElevatorState] = {
    Seq(
      Opening(floor),
      Open(floor),
      Wait(floor, 5, duration.SECONDS),
      Closing(floor),
      Closed(floor)
    )
  }

  def verifyPartOfHistory(history: Seq[ElevatorState], expectedHistory: Seq[ElevatorState]*) = {
    val flattenedHistory = historySeq(expectedHistory: _*)
    val (h, r) = history.splitAt(flattenedHistory.length)
    h shouldBe flattenedHistory
    r
  }

}