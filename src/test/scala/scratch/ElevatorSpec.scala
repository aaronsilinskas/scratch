package scratch

import org.scalatest.{FeatureSpec, GivenWhenThen, Matchers}

import scala.collection.mutable
import scala.concurrent.duration
import scala.concurrent.duration.TimeUnit
import scala.util.Random

/*
Notes for requirements and states:

Basic Elevator
- elevator pickup request
- elevator dropoff request
- open a closing door
- close an open door without waiting

Scheduling
- elevator pickup in same direction as scheduled dropoffs
- elevator pickup in different direction from scheduled dropoffs

Multiple Elevators - Super Simple Scheduling
- closest elevator going in same direction handles pickup
- closest elevator that finishes dropoffs handles pickup

Pluggable Schedulers
- custom scheduling logic can be used

Indicators
- pickup and dropoff updates should be available for UI like button lights

Maintenance
- elevators taken out of service aren't scheduled
- elevators can be manually controlled
- elevators can be put back into service

Extra stuff
- emergency call
- schedulers should take start, stop, transit, and open+close time into account
- secure floors require authentication before dropoff requests are allowed
*/
class ElevatorSpec extends FeatureSpec with GivenWhenThen with Matchers {

  feature("Elevator pickup and dropoff.") {

    scenario("A pickup request is serviced by an elevator") {
      Given("a pickup request and available elevator on different floors")
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
  }

  class Elevator(initialState: ElevatorState) {

    private val _history = mutable.Queue[ElevatorState](initialState)

    def pickup(floor: Int) = {
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
    def requestPickup(floor: Int) = {
      elevator.pickup(floor)
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

}
