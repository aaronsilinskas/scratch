package scratch

import org.scalatest.prop.TableDrivenPropertyChecks
import org.scalatest.{FeatureSpec, Matchers}

class DynamicProgrammingSpec extends FeatureSpec with Matchers with TableDrivenPropertyChecks {

  feature("Some classic DP (dynamic programming) problems") {

    scenario("Find the minimum steps to 1, where steps can be subtract 1, divide by 2, and divide by 3") {
      info("We can only divide by 2 or 3 if there is no remainder")

      val testCases = Table(
        ("value", "steps"),
        (1, 0),
        (2, 1),
        (3, 1),
        (4, 2),
        (6, 2),
        (7, 3),
        (10, 3)
      )

      forAll(testCases) {
        (value, expectedSteps) =>
          val steps = DynamicProgramming.minimumStepsTo1(value)
          steps shouldBe expectedSteps
      }
    }

    scenario("Find the longest increasing subsequence of numbers") {
      val testCases = Table(
        ("value", "longest"),
        (Array(4), 1),
        (Array(1, 2), 2),
        (Array(10, 5), 1),
        (Array(5, 3, 4, 8, 6, 7), 4)
      )

      forAll(testCases) {
        (value, expectedLongest) =>
          val longest = DynamicProgramming.longestSubsequence(value)
          longest shouldBe expectedLongest
      }
    }
  }
}
