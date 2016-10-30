package scratch

import scala.collection.{Set, mutable}

object Permutations {

  def asSet(value: String): Set[String] = {

    def iter(buffer: Array[Char], index: Int, permutations: mutable.Set[String]): Unit = {
      val remaining = buffer.length - index
      if (remaining == 1) {
        permutations += new String(buffer)
      } else {
        for (i <- index until buffer.length) {
          swap(buffer, index, i)

          iter(buffer, index + 1, permutations)

          swap(buffer, i, index)
        }
      }
    }

    def swap(buffer: Array[Char], a: Int, b: Int): Unit = {
      val tmp = buffer(a)
      buffer.update(a, buffer(b))
      buffer.update(b, tmp)
    }

    val buffer = value.toCharArray
    val permutations = mutable.Set[String]()
    for (i <- 0 until buffer.length) {
      iter(buffer, i, permutations)
    }

    permutations
  }

}
