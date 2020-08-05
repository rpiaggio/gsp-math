// Copyright (c) 2016-2020 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package gsp.math.skycalc.solver

import cats.implicits._
import cats.Eq
import cats.Show
import java.time.Duration
import monocle.Getter
import java.time.Instant
import io.chrisdavenport.cats.time._

/**
  * Representation of an interval between two points in time, including the start time and excluding the end time
  * (i.e. the interval [start, end)) and a set of operations on top of these intervals. Note that an interval
  * can not represent a single point in time (i.e. start == end) because such an interval could not contain any
  * time t for which t >= start && t < end.
  */
case class Interval(start: Instant, end: Instant) extends Ordered[Interval] {
  require(start.isBefore(end), "start of interval must be < end")

  /** True if this interval covers time t. */
  def contains(i: Instant): Boolean = i >= start && i < end

  /** True if this interval covers the given interval. */
  def contains(i: Interval): Boolean = i.start >= start && i.end <= end

  /** True if this and the other interval abut each other. */
  def abuts(other: Interval): Boolean =
    start == other.end || other.start == end

  /** True if this and the other interval overlap each other either fully or partially. */
  def overlaps(other: Interval): Boolean =
    start < other.end && end > other.start

  /** The duration of this interval. */
  def duration: Duration = Duration.between(start, end)

  /** Adds an interval to this interval. This operation is only defined if the two intervals overlap
    * or abut each, i.e. in all cases where adding the two intervals results in one single interval.
    * @param other
    * @return
    */
  def plus(other: Interval): Interval = {
    require(overlaps(other) || abuts(other))
    val s = start.min(other.start)
    val e = end.max(other.end)
    Interval(s, e)
  }

  /** The overlapping part of two intervals. */
  def overlap(other: Interval): Interval = {
    require(overlaps(other))
    val s = start.max(other.start)
    val e = end.min(other.end)
    Interval(s, e)
  }

  /** Compares to intervals and orders them by their start time. */
  def compare(that: Interval): Int =
    start.compareTo(that.start)

  /** Gets duration of interval as hours. */
  def asHours: Double = duration.toNanos.toDouble / Duration.ofHours(1).toNanos

  /** Gets duration of interval as days. */
  def asDays: Double = duration.toNanos.toDouble / Duration.ofDays(1).toNanos
}

object Interval extends IntervalOptics {

  def combine(left: List[Interval], right: List[Interval]): List[Interval] =
    if (left.isEmpty && right.isEmpty) List.empty
    else if (left.isEmpty) right
    else if (right.isEmpty) left
    else if (left.head.abuts(right.head) || left.head.overlaps(right.head)) {
      val start = left.head.start.min(right.head.start)
      val end   = left.head.end.max(right.head.end)
      // The new Interval could overlap with other Intervals on the left, so we
      // perform a combine with the rest of the left before proceeding.
      combine(combine(List(new Interval(start, end)), left.tail), right.tail)
    } else if (left.head.start < right.head.start)
      left.head +: combine(left.tail, right)
    else
      right.head +: combine(left, right.tail)

  def intersect(left: List[Interval], right: List[Interval]): List[Interval] =
    if (left.isEmpty || right.isEmpty) List.empty
    else {
      val h1 = left.head
      val h2 = right.head

      if (!h1.overlaps(h2))
        if (h1.end > h2.end)
          intersect(left, right.tail)
        else
          intersect(left.tail, right)
      else if (h1.end > h2.end)
        h1.overlap(h2) +: intersect(left, right.tail)
      else
        h1.overlap(h2) +: intersect(left.tail, right)
    }

  def reduce(left: Interval, right: List[Interval]): List[Interval] =
    reduce(List(left), right)

  def reduce(left: List[Interval], right: List[Interval]): List[Interval] =
    if (left.isEmpty) List.empty
    else if (right.isEmpty) left
    else {
      val h1 = left.head
      val h2 = right.head
      if (!h1.overlaps(h2))
        if (h1.end <= h2.start)
          h1 +: reduce(left.tail,
                       right
          )                           // no overlap and h1 is before h2 => h1 won't be touched again by any h2, add it to result
        else reduce(left, right.tail) // no overlap and h1 is after h2 => we can skip h2
      else
        reduce(reduce(h1, h2) ++ left.tail,
               right
        )                             // overlap: replace h1 with reduce(h1,h2) and continue
    }

  def reduce(i1: Interval, i2: Interval): List[Interval] =
    if (i1.start < i2.start && i1.end > i2.end)
      List(Interval(i1.start, i2.start), Interval(i2.end, i1.end))
    else if (i1.start < i2.start && i1.end <= i2.end) List(Interval(i1.start, i2.start))
    else if (i1.start >= i2.start && i1.end > i2.end) List(Interval(i2.end, i1.end))
    else List.empty

  def invert(intervals: List[Interval]): List[Interval] =
    if (intervals.size < 2) List.empty
    else
      intervals
        .sliding(2)
        .map({
          case List(i, j) => Interval(i.end, j.start)
        })
        .toList

  /**
    * Takes a sequence of intervals and transforms it into a sequence of full days (i.e. 14:00 first day to 14:00
    * on the next day) that covers all given intervals. Abutting full days are combined so that the resulting
    * sequence contains the minimal number of intervals needed to cover the original sequence.
    * @param intervals
    * @param localTime
    * @return
    */
  // def allDay(intervals: Seq[Interval], localTime: TimeZone): Seq[Interval] = {
  //   // blow intervals up to cover 24hrs (or multiples thereof); days start/end at 14hrs local time
  //   def blowUp(interval: Interval): Interval =
  //     Interval(
  //       TimeUtils.startOfDay(interval.start, localTime),
  //       TimeUtils.endOfDay(interval.end, localTime)
  //     )

  //   // note that a single interval can stretch several days (e.g. for time windows)
  //   def removeDuplicates(res: Seq[Interval], intervals: Seq[Interval]): Seq[Interval] = {
  //     intervals match {
  //       case Nil => res
  //       case head::Nil =>
  //         res :+ head
  //       case head::tail =>
  //         val h = head
  //         val t = tail.head
  //         if (h.abuts(t) || h.overlaps(t)) {
  //           removeDuplicates(res, h.plus(t) +: tail.drop(1))
  //         } else {
  //           removeDuplicates(res :+ h, tail)
  //         }
  //     }
  //   }

  //   removeDuplicates(Seq(), intervals.map(blowUp))

  // }

  /** @group Typeclass Instances */
  implicit val IntervalShow: Show[Interval] =
    Show.fromToString

  /** @group Typeclass Instances */
  implicit val IntervalEqual: Eq[Interval] =
    Eq.fromUniversalEquals
}

trait IntervalOptics { self: Interval.type =>

  /** @group Optics */
  val start: Getter[Interval, Instant] =
    Getter(_.start)

  /** @group Optics */
  val end: Getter[Interval, Instant] =
    Getter(_.end)
}
