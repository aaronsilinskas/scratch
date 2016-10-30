package scratch

import scala.collection.{Set, mutable}

object Permutations {

  def asSet(value: String): Set[String] = {

    def iterCharacters(value: String, prefix: String, next: Int, processed: mutable.BitSet): Set[String] = {
      val remainingChars = value.length - processed.size
      val nextPrefix = prefix + value(next)
      if (remainingChars == 0) {
        Set(nextPrefix)
      } else {
        processed.add(next)

        val nextPerms = for {
          i <- 0 until value.length if !processed.contains(i)
          perms <- iterCharacters(value, nextPrefix, i, processed)
        } yield perms

        processed.remove(next)

        nextPerms.toSet
      }
    }

    val processed = mutable.BitSet(value.length)
    val perms = for {
      i <- 0 until value.length
      perms <- iterCharacters(value, "", i, processed)
    } yield perms

    perms.toSet
  }

}
