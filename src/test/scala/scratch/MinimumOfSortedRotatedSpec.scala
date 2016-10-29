package scratch

import org.scalatest.prop.TableDrivenPropertyChecks
import org.scalatest.{FeatureSpec, Matchers}

import scala.math.Ordering.IntOrdering

class MinimumOfSortedRotatedSpec extends FeatureSpec with Matchers with TableDrivenPropertyChecks {

  feature("Find the minimum of a sorted randomly rotated array in log(n) time.") {

    scenario("the minimum of a Table of arrays with one or more elements will be found in log(n) time") {
      info("This code uses ScalaTest's property table for minimal code")

      val sortedRotatedArrays = Table(
        ("array", "min", "compares"),
        (Array(99), 99, 0),
        (Array(22, 7), 7, 1),
        (Array(12, 34), 12, 1),
        (Array(10, 14, 22, 34, 3, 5), 3, 2),
        (Array(44, 2, 4, 6, 9, 12, 32), 2, 4),
        (Array(1, 2, 3, 4), 1, 3),
        (Array(2, 3, 4, 1), 1, 1)
      )

      forAll(sortedRotatedArrays) {
        (array, min, compares) =>
          val compareCounter = new CompareCounter

          Minimum.ofSortedRotated(array)(compareCounter) shouldBe min

          compareCounter.count shouldBe compares
      }
    }

    scenario("with case class, the minimum of a set of Test Cases of arrays with one or more elements will be found in log(n) time") {
      info("This code uses a case class for test cases, which I feel is faster to read and understand")

      case class TestCase(array: Array[Int], min: Int, compares: Int)
      val sortedRotatedArrays = List(
        TestCase(Array(99), min = 99, compares = 0),
        TestCase(Array(22, 7), min = 7, compares = 1),
        TestCase(Array(12, 34), min = 12, compares = 1),
        TestCase(Array(10, 14, 22, 34, 3, 5), min = 3, compares = 2),
        TestCase(Array(44, 2, 4, 6, 9, 12, 32), min = 2, compares = 4),
        TestCase(Array(1, 2, 3, 4), min = 1, compares = 3),
        TestCase(Array(2, 3, 4, 1), min = 1, compares = 1)
      )

      sortedRotatedArrays foreach { testCase =>
        val compareCounter = new CompareCounter

        Minimum.ofSortedRotated(testCase.array)(compareCounter) shouldBe testCase.min

        compareCounter.count shouldBe testCase.compares
      }
    }

    class CompareCounter extends IntOrdering {
      var count = 0

      override def compare(x: Int, y: Int) = {
        count += 1
        super.compare(x, y)
      }

    }
  }
}
