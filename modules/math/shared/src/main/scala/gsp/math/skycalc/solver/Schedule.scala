// Copyright (c) 2016-2020 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package gsp.math.skycalc.solver

import cats.implicits._
import cats.Eq
import cats.Monoid
import cats.Show
import java.time.Instant
import java.time.Duration
import io.chrisdavenport.cats.time._
import monocle.Getter
import monocle.Prism
import gsp.math.optics.SplitEpi

/**
  * An arbitrary number of Intervals.
  * The intervals are sorted by their start time and don't overlap or abut, i.e. the Schedule is always represented
  * by the smallest possible set of intervals.
  */
sealed abstract case class Schedule protected (intervals: List[Interval]) {

  /** True if the Schedule (i.e. any of its intervals) contains instant i. */
  def contains(i: Instant) = intervals.exists(_.contains(i))

  def isEmpty: Boolean             = intervals.isEmpty
  def never: Boolean               = isEmpty
  def headOption: Option[Interval] = intervals.headOption
  def tail: Schedule               =
    intervals match {
      case _ :: t => Schedule.unsafe(t)
      case Nil    => Schedule.Never
    }
  def earliest: Option[Instant]    = headOption.map(_.start)
  def latest: Option[Instant]      = intervals.lastOption.map(_.end)
  def duration: Duration           = intervals.foldMap(_.duration)

  /**
    * Adds a Schedule to this Schedule.
    * @param other
    * @return
    */
  def extend(other: Schedule): Option[Schedule] =
    //This will be true if one of the lists is empty or if the condition holds
    if (latest.zip(other.earliest).forall { case (leftEnd, rightStart) => leftEnd < rightStart })
      Schedule(intervals ++ other.intervals)
    else
    // Both lists have at least one element
    if (intervals.last.abuts(other.intervals.head))
      Schedule(
        (intervals.init :+ intervals.last.join(other.intervals.head).get) ++ other.intervals.tail
      )
    else
      none

  /**
    * Adds an interval to this Schedule.
    * @param interval
    * @return
    */
  def extend(interval: Interval): Option[Schedule] =
    extend(Schedule.single(interval))

  /**
    * True if any part of this Schedule overlaps with the given interval.
    * @param interval
    * @return
    */
  def overlaps(interval: Interval): Boolean =
    intervals.exists(i => i.overlaps(interval))

  /**
    * Restricts a Schedule to only the intervals in the given interval.
    * @param interval
    * @return
    */
  def restrictTo(interval: Interval): Schedule =
    Schedule.unsafe(
      intervals
        .filter(i => i.end >= interval.start && i.start <= interval.end)
        .map(i => Interval.unsafe(i.start.max(interval.start), i.end.min(interval.end)))
    )

  // def allDay(localTime: TimeZone): Schedule = {
  //   // blow intervals up to cover 24hrs (or multiples thereof); days start/end at 14hrs local time
  //   def blowUp(interval: Interval): Interval =
  //     Interval(
  //       TimeUtils.startOfDay(interval.start, localTime),
  //       TimeUtils.endOfDay(interval.end, localTime)
  //     )

  //   // note that a single interval can stretch several days (e.g. for time windows)
  //   def removeDuplicates(res: Seq[Interval], intervals: Seq[Interval]): Seq[Interval] =
  //     intervals match {
  //       case Nil          => res
  //       case head :: Nil  =>
  //         res :+ head
  //       case head :: tail =>
  //         val h = head
  //         val t = tail.head
  //         if (h.abuts(t) || h.overlaps(t))
  //           removeDuplicates(res, h.plus(t) +: tail.drop(1))
  //         else
  //           removeDuplicates(res :+ h, tail)
  //     }

  //   if (this == Schedule.Always)
  //     Schedule.Always // Always can not be "blown" up, don't try
  //   else
  //     Schedule(removeDuplicates(Seq(), intervals.map(blowUp)))

  // }

  /**
    * Combines two Schedules.
    * Merges all overlapping and abutting intervals.
    */
  def union(other: Schedule): Schedule = Schedule.union(this, other)

  /**
    * Intersects a Schedule with another one.
    * The result will contain all intervals of this Schedule which are covered by both Schedules.
    */
  def intersection(other: Schedule): Schedule = Schedule.intersection(this, other)

  /**
    * Remove from this Schedule intersections with another one.
    * The result will contain all intervals of this Schedule which are NOT covered by the the given Schedule.
    * @param s
    * @return
    */
  def diff(other: Schedule): Schedule = Schedule.diff(this, other)

  def gaps: Schedule =
    if (intervals.size < 2) Schedule.Never
    else
      Schedule.unsafe(
        intervals
          .sliding(2)
          .map({
            case List(i, j) => Interval.unsafe(i.end, j.start)
          })
          .toList
      )

}

/**
  * Companion objects with convenience constructors.
  */
object Schedule extends ScheduleOptics {

  /** Schedule that is always true (i.e. for any time t). */
  val Always = unsafe(List(Interval.Always))

  /** Schedule that is never true. */
  val Never = unsafe(List.empty)

  def single(interval: Interval): Schedule = new Schedule(List(interval)) {}

  /** Convenience constructors. */
  def apply(): Schedule = Never
  def apply(interval:  Interval): Schedule = single(interval)
  def apply(start:     Instant, end: Instant): Option[Schedule] =
    Interval(start, end).map(single)
  def apply(intervals: List[Interval]): Option[Schedule] =
    fromDisjointSortedIntervals.getOption(intervals)

  def unsafe(intervals: List[Interval]): Schedule =
    apply(intervals).get
  def unsafe(start:     Instant, end: Instant): Schedule =
    apply(start, end).get

  def union(left: Schedule, right: Schedule): Schedule =
    (left.intervals, right.intervals) match {
      case (Nil, _)                        => right
      case (_, Nil)                        => left
      case (leftHead :: _, rightHead :: _) =>
        leftHead
          .join(rightHead)
          .fold( // Heads can't be joined: they don't about or overlap
            if (leftHead.start < rightHead.start)
              unsafe(leftHead +: union(left.tail, right).intervals)
            else
              unsafe(rightHead +: union(left, right.tail).intervals)
          )(joined =>
            // The new Interval could overlap with other Intervals on the left Schedule, so we
            // perform a combine with the rest of the left before proceeding.
            union(union(single(joined), left.tail), right.tail)
          )
    }

  def intersection(left: Schedule, right: Schedule): Schedule =
    (left.intervals, right.intervals) match {
      case (Nil, _)                        => Never
      case (_, Nil)                        => Never
      case (leftHead :: _, rightHead :: _) =>
        leftHead
          .intersection(rightHead)
          .fold( // Heads can't be joined: they don't about or overlap
            if (leftHead.end > rightHead.end)
              intersection(left, unsafe(right.intervals.tail))
            else
              intersection(left.tail, right)
          )(intersected =>
            if (leftHead.end > rightHead.end)
              unsafe(intersected +: intersection(left, right.tail).intervals)
            else
              unsafe(intersected +: intersection(left.tail, right).intervals)
          )
    }

  def diff(left: Schedule, right: Schedule): Schedule =
    (left.intervals, right.intervals) match {
      case (Nil, _)                        => Never
      case (_, Nil)                        => left
      case (leftHead :: _, rightHead :: _) =>
        if (!leftHead.overlaps(rightHead))
          if (leftHead.end <= rightHead.start)
            // no overlap and leftHead is before rightHead => leftHead won't be touched again by any rightHead, add it to result
            unsafe(leftHead +: diff(left.tail, right).intervals)
          else
            // no overlap and leftHead is after rightHead => we can skip rightHead
            diff(left, right.tail)
        else
          // overlap: replace leftHead with leftHead.diff(rightHead) and continue
          diff(unsafe(leftHead.diff(rightHead) ++ left.tail.intervals), right)
    }

  /** @group Typeclass Instances */
  implicit val ScheduleShow: Show[Schedule] =
    Show.fromToString

  /** @group Typeclass Instances */
  implicit val ScheduleEqual: Eq[Schedule] = Eq.fromUniversalEquals

  /** @group Typeclass Instances */
  implicit object ScheduleMonoid extends Monoid[Schedule] {
    def empty: Schedule = Schedule.Never

    def combine(x: Schedule, y: Schedule): Schedule = x.union(y)
  }
}

trait ScheduleOptics { self: Schedule.type =>

  /** @group Optics */
  val intervals: Getter[Schedule, List[Interval]] =
    Getter(_.intervals)

// SplitEpi from any list of intervals and normalize?

  /** @group Optics */
  val fromDisjointSortedIntervals: Prism[List[Interval], Schedule] =
    Prism { intervals: List[Interval] =>
      if (
        intervals.sliding(2).forall {
          case List(a, b) => a.end < b.start
          case _          => true // Zero or one intervals
        }
      )
        (new Schedule(intervals) {}).some
      else
        none
    }(_.intervals)

  /** @group Optics */
  val fromIntervals: SplitEpi[List[Interval], Schedule] =
    SplitEpi[List[Interval], Schedule](
      _.foldLeft(Schedule.Never)((s, i) => s.union(Schedule.single(i))),
      _.intervals
    )
}
