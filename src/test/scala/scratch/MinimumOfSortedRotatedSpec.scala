package scratch

import org.scalatest.prop.TableDrivenPropertyChecks
import org.scalatest.{FeatureSpec, Matchers}

import scala.util.Random

class MinimumOfSortedRotatedSpec extends FeatureSpec with Matchers with TableDrivenPropertyChecks {

  feature("Find the minimum of a sorted randomly rotated array in log(n) time.") {

    scenario("The minimum of a Table of arrays with one or more elements will be found in log(n) time") {
      val sortedRotatedArrays = Table(
        ("description", "min", "array"),
        ("single element", 99, Array(99)),
        ("two elements, min on right", 7, Array(22, 7)),
        ("two elements, min on left", 12, Array(12, 34)),
        ("min on right", 3, Array(10, 14, 22, 34, 3, 5)),
        ("min on left", 2, Array(44, 2, 4, 6, 9, 12, 32)),
        ("min in center", 5, Array(12, 32, 44, 5, 6, 9, 10)),
        ("min as first element", 1, Array(1, 2, 3, 4)),
        ("min as last element", 1, Array(2, 3, 4, 1))
      )

      forAll(sortedRotatedArrays) {
        (description, min, array) =>
          val maximumCompares = maximumComparesForLogN(array.length)
          val compareCounter = new CompareCounter(Ordering.Int)

          Minimum.ofSortedRotated(array)(compareCounter) shouldBe min

          compareCounter.count should be <= maximumCompares
      }
    }

    scenario("Attempting to find the minimum of an empty array will error") {
      val array = Array.empty[Int]
      intercept[IllegalArgumentException] {
        Minimum.ofSortedRotated(array)(Ordering.Int)
      }
    }

    scenario("The minimum of a large random array of sorted rotated floats will be found in log(n) time") {
      val array = randomSortedArrayOfFloats
      rotateArrayByRandomAmount(array)
      val minimum = array.min
      val maximumCompares = maximumComparesForLogN(array.length)

      val compareCounter = new CompareCounter(Ordering.Float)

      Minimum.ofSortedRotated(array)(compareCounter) shouldBe minimum

      compareCounter.count should be <= maximumCompares
    }


    class CompareCounter[A](o: Ordering[A]) extends Ordering[A] {
      var count = 0

      override def compare(x: A, y: A) = {
        count += 1
        o.compare(x, y)
      }
    }

    def maximumComparesForLogN(n: Int): Int = {
      Math.round(Math.ceil(Math.log10(n) / Math.log10(2.0))).toInt + 1
    }

    def randomSortedArrayOfFloats: Array[Float] = {
      val arraySize = 1000000 + Random.nextInt(1000000)
      val randomSeq = (0 to arraySize).map(_ => Random.nextFloat())
      randomSeq.sorted.toArray
    }

    def rotateArrayByRandomAmount[A](array: Array[A]): Unit = {

      def reverse(array: Array[A], start: Int, end: Int): Unit = {
        val swapsNeeded = (end - start) / 2
        for (i <- 0 until swapsNeeded) {
          val a = start + i
          val b = end - i - 1

          val tmp = array(a)
          array.update(a, array(b))
          array.update(b, tmp)
        }
      }

      val rotations = Random.nextInt(array.length)
      
      reverse(array, 0, rotations)
      reverse(array, rotations, array.length)
      reverse(array, 0, array.length)
    }
  }
}
