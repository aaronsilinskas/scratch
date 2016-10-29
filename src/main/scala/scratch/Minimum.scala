package scratch

import scala.annotation.tailrec

object Minimum {

  def ofSortedRotated[A](a: Array[A])(ordering: Ordering[A]): A = {
    ofSortedRotated(a, 0, a.length)(ordering)
  }

  @tailrec
  def ofSortedRotated[A](a: Array[A], start: Int, end: Int)(ordering: Ordering[A]): A = {
    val remainingElements = end - start

    if (remainingElements == 1) {
      a(start)
    } else if (remainingElements == 2) {
      ordering.min(a(start), a(end - 1))
    } else {
      val pivot = start + (remainingElements / 2)

      if (ordering.gt(a(pivot), a(end - 1))) {
        ofSortedRotated(a, pivot + 1, end)(ordering)
      } else {
        ofSortedRotated(a, start, pivot + 1)(ordering)
      }
    }
  }

}
