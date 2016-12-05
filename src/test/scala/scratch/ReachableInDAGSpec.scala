package scratch

import org.scalatest.prop.TableDrivenPropertyChecks
import org.scalatest.{FeatureSpec, Matchers}
import scratch.ReachableDAG._

class ReachableInDAGSpec extends FeatureSpec with Matchers with TableDrivenPropertyChecks {

  feature("Find out if a node is reachable from another node in a directed acyclic graph") {

    val testEdges = List(
      "A" -> "C",
      "A" -> "B",
      "B" -> "C",
      "C" -> "D",
      "D" -> "E",
      "D" -> "F",
      "C" -> "G",
      "F" -> "G"
    )
    val testLabelsToEdges = edgesToLabelMap(testEdges).usingMapChain
    val testGraph = new TailRecDAG(testLabelsToEdges)

    scenario("Convert a series of edges to a graph") {
      testGraph.edges("A") shouldBe Set("B", "C")
      testGraph.edges("B") shouldBe Set("C")
      testGraph.edges("C") shouldBe Set("D", "G")
      testGraph.edges("D") shouldBe Set("E", "F")
    }

    scenario("Reachable and unreachable nodes from zero to multiple edges") {
      val testCases = Table(
        ("description", "from", "to", "reachable"),
        ("Same node", "C", "C", true),
        ("Wrong direction", "C", "A", false),
        ("Direct edge", "C", "D", true),
        ("Traverse multiple edges", "A", "D", true),
        ("Traverse even more edges", "B", "G", true)
      )

      forAll(testCases) {
        (description, from, to, expectedReachable) =>
          testGraph.isReachable(from, to) shouldBe expectedReachable
      }
    }

    scenario("All edge to graph implementations should give the same results") {
      edgesToLabelMap(testEdges).usingMapChain shouldBe testLabelsToEdges

      edgesToLabelMap(testEdges).usingForComprehension shouldBe testLabelsToEdges

      edgesToLabelMap(testEdges).toScarePeopleAwayFromScala shouldBe testLabelsToEdges
    }

    scenario("All reachable implementations should give the same results") {
      val nrDAG = new NaiveRecursiveDAG(testLabelsToEdges)
      nrDAG.isReachable("A", "G") shouldBe true
      nrDAG.isReachable("B", "A") shouldBe false

      val mqndcDAG = new MutableQueueAndNoDuplicateChecksDAG(testLabelsToEdges)
      mqndcDAG.isReachable("A", "G") shouldBe true
      mqndcDAG.isReachable("B", "A") shouldBe false

      val trDAG = new TailRecDAG(testLabelsToEdges)
      trDAG.isReachable("A", "G") shouldBe true
      trDAG.isReachable("B", "A") shouldBe false
    }
  }
}
