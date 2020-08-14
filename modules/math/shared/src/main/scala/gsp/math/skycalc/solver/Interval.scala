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
import monocle.Prism
import gsp.math.optics.Format
import monocle.Iso

/**
  * Representation of an interval between two points in time, including the start time and excluding the end time
  * (i.e. the interval [start, end)) and a set of operations on top of these intervals. Note that an interval
  * can not represent a single point in time (i.e. start == end) because such an interval could not contain any
  * time t for which t >= start && t < end.
  */
sealed abstract case class Interval protected (start: Instant, end: Instant)
    extends Ordered[Interval] {
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
  def join(other: Interval): Option[Interval] =
    if (overlaps(other) || abuts(other))
      Interval(start.min(other.start), end.max(other.end))
    else
      none

  /** The overlapping part of two intervals. */
  def intersection(other: Interval): Option[Interval] =
    if (overlaps(other))
      Interval(start.max(other.start), end.min(other.end))
    else
      none

  def diff(other: Interval): List[Interval] =
    if (this.start < other.start && this.end > other.end)
      List(Interval.unsafe(this.start, other.start), Interval.unsafe(other.end, this.end))
    else if (this.start < other.start && this.end <= other.end)
      List(Interval.unsafe(this.start, other.start))
    else if (this.start >= other.start && this.end > other.end)
      List(Interval.unsafe(other.end, this.end))
    else List.empty

  def diff(other: Schedule): Schedule =
    Schedule.single(this).diff(other)

  /** Compares to intervals and orders them by their start time. */
  def compare(that: Interval): Int =
    start.compareTo(that.start)

  /** Gets duration of interval as hours. */
  // def asHours: Double = duration.toNanos.toDouble / Duration.ofHours(1).toNanos

  /** Gets duration of interval as days. */
  // def asDays: Double = duration.toNanos.toDouble / Duration.ofDays(1).toNanos
}

object Interval extends IntervalOptics {
  val Always: Interval = unsafe(Instant.MIN, Instant.MAX)

  def apply(start: Instant, end: Instant): Option[Interval] =
    fromOrderedInstants.getOption((start, end))

  def unsafe(start: Instant, end: Instant): Interval =
    apply(start, end).get

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

  /** @group Optics */
  val duration: Getter[Interval, Duration] =
    Getter(_.duration)

  /** @group Optics */
  val fromOrderedInstants: Prism[(Instant, Instant), Interval] =
    Prism[(Instant, Instant), Interval] {
      case (start: Instant, end: Instant) =>
        if (start < end) (new Interval(start, end) {}).some else none
    }(i => (i.start, i.end))

  /** @group Optics */
  val fromInstants: Format[(Instant, Instant), Interval] =
    Format[(Instant, Instant), Interval](
      {
        case (start: Instant, end: Instant) =>
          if (start < end) (new Interval(start, end) {}).some
          else if (start > end) (new Interval(end, start) {}).some
          else none
      },
      i => (i.start, i.end)
    )

  /** @group Optics */
  val startDuration: Iso[(Instant, Duration), Interval] =
    Iso[(Instant, Duration), Interval] {
      case (start, duration) => new Interval(start, start.plus(duration)) {}
    }(i => (i.start, i.duration))

}
