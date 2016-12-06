package scratch

object MeetingMerger {

  type Meeting = (Int, Int)

  def startOfMeeting(m: Meeting): Int = m._1

  def endOfMeeting(m: Meeting): Int = m._2

  def meetingsOverlap(m1: Meeting, m2: Meeting): Boolean = {
    startOfMeeting(m1) <= endOfMeeting(m2) && endOfMeeting(m1) >= startOfMeeting(m2)
  }

  def merge(meetings: Set[Meeting]): Set[Meeting] = {
    val sorted = meetings.toList.sortBy(startOfMeeting)

    val merged = sorted.foldLeft(List.empty[Meeting]) { (merged, next) =>
      if (merged.isEmpty) {
        List(next)
      } else {
        val lastMergedMeeting = merged.last
        if (meetingsOverlap(lastMergedMeeting, next)) {
          val minStart = Math.min(startOfMeeting(lastMergedMeeting), startOfMeeting(next))
          val maxEnd = Math.max(endOfMeeting(lastMergedMeeting), endOfMeeting(next))
          val mergedMeeting = (minStart, maxEnd)
          merged.dropRight(1) :+ mergedMeeting
        } else {
          merged :+ next
        }
      }
    }

    merged.toSet
  }
  
}
