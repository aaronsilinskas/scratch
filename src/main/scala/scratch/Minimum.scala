package scratch

import scala.annotation.tailrec

object Minimum {

  def ofSortedRotated[A](a: Array[A])(ordering: Ordering[A]): A = {
    ofSortedRotated(a, 0, a.length)(ordering)
  }

  @tailrec
  def ofSortedRotated[A](a: Array[A], start: Int, end: Int)(ordering: Ordering[A]): A = {
    require(a.length > 0)

    val remainingElements = end - start
    remainingElements match {
      case 1 => a(start)
      case 2 => ordering.min(a(start), a(end - 1))
      case n =>
        val pivot = start + (n / 2)

        if (ordering.gt(a(pivot), a(end - 1))) {
          ofSortedRotated(a, pivot + 1, end)(ordering)
        } else {
          ofSortedRotated(a, start, pivot + 1)(ordering)
        }
    }
  }

}
