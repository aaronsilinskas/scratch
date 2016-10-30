package scratch

import org.scalatest.prop.TableDrivenPropertyChecks
import org.scalatest.{FeatureSpec, Matchers}

import scala.collection.Set
import scala.util.Random

class StringPermutationsSpec extends FeatureSpec with Matchers with TableDrivenPropertyChecks {

  feature("Find all of the permutations of characters in a string") {

    scenario("Find the permutations of a set of small strings") {
      val testCases = Table(
        ("value", "permutations"),
        ("a", Set("a")),
        ("ab", Set("ab", "ba")),
        ("abc", Set("abc", "acb", "bac", "bca", "cab", "cba"))
      )

      forAll(testCases) {
        (value, expectedPermutations) =>
          val permutations = Permutations.asSet(value)
          permutations should contain theSameElementsAs expectedPermutations
      }
    }

    scenario("Find the permutations of a random string") {
      val value = Random.nextString(7)
      val expectedPermutations = value.permutations.toSet

      val permutations = Permutations.asSet(value)

      permutations should contain theSameElementsAs expectedPermutations
    }
  }
}
