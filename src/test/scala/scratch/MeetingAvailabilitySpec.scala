package scratch

import org.scalatest.prop.TableDrivenPropertyChecks
import org.scalatest.{FeatureSpec, Matchers}
import scratch.MeetingAvailability._

class MeetingAvailabilitySpec extends FeatureSpec with Matchers with TableDrivenPropertyChecks {

  feature("Find next availability between two calendars for a variable duration.") {
    info("All times are measured in hours relative to the current time for an easier demo than real date+times")
    info("Calendar meetings are always sorted by start date, ascending")

    scenario("Both calendars have immediate availability before meetings") {
      val duration = 1
      val calendar1 = List(TimeRange(2.5, 4))
      val calendar2 = List(TimeRange(4, 5))

      findAvailability(duration, calendar1, calendar2) shouldBe TimeRange(0, 1)
    }

    scenario("One calendar doesn't have enough time before a meeting") {
      val duration = 2
      val calendar1 = List(TimeRange(1, 4))
      val calendar2 = List(TimeRange(3, 5))

      findAvailability(duration, calendar1, calendar2) shouldBe TimeRange(5, 7)
    }

    scenario("Both calendars have exactly the duration available between meetings") {
      val duration = 0.5
      val calendar1 = List(TimeRange(1, 2), TimeRange(2.5, 5))
      val calendar2 = List(TimeRange(0, 2), TimeRange(2.5, 5))

      findAvailability(duration, calendar1, calendar2) shouldBe TimeRange(2, 2.5)
    }

    scenario("No availability until after all meetings") {
      val duration = 1.5
      val calendar1 = List(TimeRange(1, 2), TimeRange(2.5, 4), TimeRange(4.5, 6))
      val calendar2 = List(TimeRange(0, 3))

      findAvailability(duration, calendar1, calendar2) shouldBe TimeRange(6, 7.5)
    }

    scenario("One calendar has a 3 month sabbatical") {
      val duration = 1
      val calendar1 = List.empty[TimeRange]
      val calendar2 = List(TimeRange(0, 2160))

      findAvailability(duration, calendar1, calendar2) shouldBe TimeRange(2160, 2161)
    }

    scenario("Calendars are triple-booked") {
      val duration = 2
      val calendar1 = List(TimeRange(1, 3), TimeRange(1, 2), TimeRange(1.5, 4), TimeRange(7, 8))
      val calendar2 = List(TimeRange(0, 3), TimeRange(1, 1.5), TimeRange(1, 3))

      findAvailability(duration, calendar1, calendar2) shouldBe TimeRange(4, 6)
    }

    scenario("Empty calendars have immediate availability") {
      val duration = 4
      val calendar1 = List.empty[TimeRange]
      val calendar2 = List.empty[TimeRange]

      findAvailability(duration, calendar1, calendar2) shouldBe TimeRange(0, 4)
    }

    scenario("Past meetings are ignored, availability will only be in the future") {
      val duration = 1
      val calendar1 = List(TimeRange(-4, -3), TimeRange(-2, 1), TimeRange(3, 4))
      val calendar2 = List(TimeRange(-10, -3), TimeRange(2, 3))

      findAvailability(duration, calendar1, calendar2) shouldBe TimeRange(1, 2)
    }
  }
}
