package scratch

import org.scalatest.prop.TableDrivenPropertyChecks
import org.scalatest.{FeatureSpec, Matchers}

class MeetingMergerSpec extends FeatureSpec with Matchers with TableDrivenPropertyChecks {

  feature("Merge a set of meeting times in O(nlog(n)) time") {
    info("Meeting start and end times are indexed in 30 minute intervals from 9:00am")
    info("There is no upper bound on meeting times to support other values like unix timestamps")

    val testCases = Table(
      ("description", "meetings", "expected"),
      ("Overlapping", Set((1, 3), (2, 4)), Set((1, 4))),
      ("End touching start", Set((1, 2), (2, 3)), Set((1, 3))),
      ("Start touching end", Set((4, 6), (1, 4)), Set((1, 6))),
      ("Unordered with multiple results", Set((0, 1), (3, 5), (4, 8), (10, 12), (9, 10)), Set((0, 1), (3, 8), (9, 12)))
    )

    scenario("Meetings from a set of test cases are merged correctly") {
      forAll(testCases) {
        (description, meetings, expectedResult) =>
          val mergedMeetings = MeetingMerger.merge(meetings)
          mergedMeetings should contain allElementsOf expectedResult
      }
    }
  }

}
