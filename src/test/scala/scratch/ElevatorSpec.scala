package scratch

import org.scalatest.FeatureSpec

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
class ElevatorSpec extends FeatureSpec {
  

}
