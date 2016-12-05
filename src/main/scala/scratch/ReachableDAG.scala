package scratch

import scala.annotation.tailrec
import scala.collection.mutable

abstract class ReachableDAG(val edges: Map[String, Set[String]]) {
  def isReachable(from: String, to: String): Boolean
}

class NaiveRecursiveDAG(edges: Map[String, Set[String]]) extends ReachableDAG(edges) {

  def isReachable(from: String, to: String): Boolean = {
    if (from == to) {
      true
    } else {
      val fromEdges = edges.getOrElse(from, Set.empty)
      fromEdges.exists(isReachable(_, to))
    }
  }
}

class MutableQueueAndNoDuplicateChecksDAG(edges: Map[String, Set[String]]) extends ReachableDAG(edges) {

  def isReachable(from: String, to: String): Boolean = {
    val remaining = mutable.Queue[String](from)
    val visited = mutable.Set[String]()
    var reachable = false

    while (remaining.nonEmpty) {
      val head = remaining.dequeue()
      if (head == to) {
        reachable = true
        remaining.clear()
      } else {
        visited += head
        val headEdges = edges.getOrElse(head, Set.empty)
        val unvisitedEdged = headEdges.diff(visited)
        remaining ++= unvisitedEdged
      }
    }

    reachable
  }
}

class TailRecDAG(edges: Map[String, Set[String]]) extends ReachableDAG(edges) {

  def isReachable(from: String, to: String): Boolean = {
    @tailrec
    def iter(remaining: List[String], visited: Set[String]): Boolean = {
      remaining match {
        case head :: tail =>
          if (head == to) {
            true
          } else {
            val headEdges = edges.getOrElse(head, Set.empty)
            val unvisitedEdged = headEdges.diff(visited)
            iter(tail ++ unvisitedEdged, visited + head)
          }
        case _ =>
          false
      }
    }

    iter(List(from), Set.empty)
  }
}

object ReachableDAG {

  def edgesToLabelMap(edges: List[(String, String)]) = new {

    def usingMapChain: Map[String, Set[String]] = {
      val labelToEdgePairs = edges.groupBy(_._1)
      labelToEdgePairs.map {
        case (label, edgePairs) =>
          (label, edgePairs.map(_._2).toSet)
      }
    }

    def toScarePeopleAwayFromScala: Map[String, Set[String]] = {
      edges.groupBy(_._1).map { case (l, ep) => (l, ep.map(_._2).toSet) }
    }

    def usingForComprehension: Map[String, Set[String]] = {
      for {
        (label, edgePairs) <- edges.groupBy(_._1)
      } yield {
        val edgeLabels = edgePairs.map(_._2).toSet
        (label, edgeLabels)
      }
    }
  }
}
