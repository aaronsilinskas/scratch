package scratch

import scala.annotation.tailrec

case class TimeRange(start: Double, end: Double) {

  def endsBefore(time: Double): Boolean = {
    end <= time
  }

  def startsBefore(time: Double): Boolean = {
    start < time
  }

  def overlap(other: TimeRange): TimeRange = {
    TimeRange(Math.max(start, other.start), Math.min(end, other.end))
  }

  def length: Double = {
    end - start
  }

}

object MeetingAvailability {

  def findAvailability(duration: Double, scheduledMeetings1: List[TimeRange], scheduledMeetings2: List[TimeRange]): TimeRange = {

    @tailrec
    def findNextAvailabilityAndMeetingsAfter(minimumStartTime: Double, meetings: List[TimeRange]): (TimeRange, List[TimeRange]) = {
      meetings match {
        case head :: tail =>
          if (head.endsBefore(minimumStartTime)) {
            findNextAvailabilityAndMeetingsAfter(minimumStartTime, tail)
          } else if (head.startsBefore(minimumStartTime + duration)) {
            findNextAvailabilityAndMeetingsAfter(head.end, tail)
          } else {
            (TimeRange(minimumStartTime, head.start), meetings)
          }
        case _ =>
          (TimeRange(minimumStartTime, Double.MaxValue), List.empty)
      }
    }

    @tailrec
    def iter(minimumStartTime: Double, meetingsRemaining1: List[TimeRange], meetingsRemaining2: List[TimeRange]): TimeRange = {
      val (nextAvailable1, meetingsAfter1) = findNextAvailabilityAndMeetingsAfter(minimumStartTime, meetingsRemaining1)
      val (nextAvailable2, meetingsAfter2) = findNextAvailabilityAndMeetingsAfter(minimumStartTime, meetingsRemaining2)
      val availabilityOverlap = nextAvailable1.overlap(nextAvailable2)
      if (availabilityOverlap.length >= duration) {
        TimeRange(availabilityOverlap.start, availabilityOverlap.start + duration)
      } else {
        val nextMinimumStartTime = Math.max(nextAvailable1.start, nextAvailable2.start)
        iter(nextMinimumStartTime, meetingsAfter1, meetingsAfter2)
      }
    }

    iter(0, scheduledMeetings1, scheduledMeetings2)
  }
}
