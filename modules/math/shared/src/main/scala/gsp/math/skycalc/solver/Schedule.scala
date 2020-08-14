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
import java.time.ZoneId
import java.time.LocalTime
import scala.annotation.tailrec

/**
  * An arbitrary number of Intervals.
  * The intervals are sorted by their start time and don't overlap or abut, i.e. the Schedule is always represented
  * by the smallest possible set of intervals.
  */
sealed abstract case class Schedule protected (intervals: List[Interval]) {

  /** True if the Schedule (i.e. any of its intervals) contains instant i. */
  def contains(i: Instant) = intervals.exists(_.contains(i))

  lazy val isEmpty: Boolean             = intervals.isEmpty
  lazy val never: Boolean               = isEmpty
  lazy val headOption: Option[Interval] = intervals.headOption
  lazy val tail: Schedule               =
    intervals match {
      case _ :: t => Schedule.unsafe(t)
      case Nil    => Schedule.Never
    }
  lazy val earliest: Option[Instant]    = headOption.map(_.start)
  lazy val latest: Option[Instant]      = intervals.lastOption.map(_.end)
  lazy val duration: Duration           = intervals.foldMap(_.duration)

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

  // Map intervals, sort and join them when they overlap.
  def mapIntervals(f: Interval => Interval): Schedule =
    Schedule.fromIntervals.get(intervals.map(f))

  def toFullDays(zone: ZoneId, startOfDay: LocalTime): Schedule =
    mapIntervals(_.toFullDays(zone, startOfDay))

  /**
    * Combines two Schedules.
    * Merges all overlapping and abutting intervals.
    */
  def union(other: Schedule): Schedule = Schedule.union(this, other)

  def union(interval: Interval): Schedule = union(Schedule.single(interval))

  /**
    * Intersects a Schedule with another one.
    * The result will contain all intervals of this Schedule which are covered by both Schedules.
    */
  def intersection(other: Schedule): Schedule = Schedule.intersection(this, other)

  def intersection(interval: Interval): Schedule = intersection(Schedule.single(interval))

  /**
    * Remove from this Schedule intersections with another one.
    * The result will contain all intervals of this Schedule which are NOT covered by the the given Schedule.
    * @param s
    * @return
    */
  def diff(other: Schedule): Schedule = Schedule.diff(this, other)

  def diff(interval: Interval): Schedule = diff(Schedule.single(interval))

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

  /** Helpers. In recursions, reversed accumulators are used to avoid incurring in quadratic costs. */
  private def union(s1: Schedule, s2: Schedule): Schedule = {

    @tailrec
    def joinInitial(
      head: Interval,
      tail: List[Interval]
    ): List[Interval] =
      tail match {
        case Nil                  => List(head)
        case tailHead :: tailTail =>
          head.join(tailHead) match {
            case None         => head +: tail
            case Some(joined) => joinInitial(joined, tailTail)
          }
      }

    @tailrec
    def go(
      left:  List[Interval],
      right: List[Interval],
      accum: List[Interval]
    ): List[Interval] =
      (left, right) match {
        case (Nil, _)                        => right.reverse ++ accum
        case (_, Nil)                        => left.reverse ++ accum
        case (leftHead :: _, rightHead :: _) =>
          leftHead.join(rightHead) match {
            case None         => // Heads can't be joined: they don't abut or overlap
              if (leftHead.start < rightHead.start)
                go(left.tail, right, leftHead :: accum)
              else
                go(left, right.tail, rightHead :: accum)
            case Some(joined) =>
              // The new Interval could overlap with other Intervals on the left Schedule, so we
              // combine it with the rest of the left list before proceeding.
              go(joinInitial(joined, left.tail), right.tail, accum)
          }
      }

    unsafe(go(s1.intervals, s2.intervals, List.empty).reverse)
  }

  private def intersection(s1: Schedule, s2: Schedule): Schedule = {

    @tailrec
    def go(
      left:  List[Interval],
      right: List[Interval],
      accum: List[Interval]
    ): List[Interval] =
      (left, right) match {
        case (Nil, _)                        => accum
        case (_, Nil)                        => accum
        case (leftHead :: _, rightHead :: _) =>
          leftHead.intersection(rightHead) match {
            case None              => // Heads can't be joined: they don't about or overlap
              if (leftHead.end > rightHead.end)
                go(left, right.tail, accum)
              else
                go(left.tail, right, accum)
            case Some(intersected) =>
              if (leftHead.end > rightHead.end)
                go(left, right.tail, intersected :: accum)
              else
                go(left.tail, right, intersected :: accum)
          }
      }

    unsafe(go(s1.intervals, s2.intervals, List.empty).reverse)
  }

  private def diff(s1: Schedule, s2: Schedule): Schedule = {
    @tailrec
    def go(left: List[Interval], right: List[Interval], accum: List[Interval]): List[Interval] =
      (left, right) match {
        case (Nil, _)                        => accum
        case (_, Nil)                        => left.reverse ++ accum
        case (leftHead :: _, rightHead :: _) =>
          if (!leftHead.overlaps(rightHead))
            if (leftHead.end <= rightHead.start)
              // no overlap and leftHead is before rightHead => leftHead won't be touched again by any rightHead, add it to result
              go(left.tail, right, leftHead :: accum)
            else
              // no overlap and leftHead is after rightHead => we can skip rightHead
              go(left, right.tail, accum)
          else
            // overlap: replace leftHead with leftHead.diff(rightHead) and continue
            go(leftHead.diff(rightHead) ++ left.tail, right, accum)
      }

    unsafe(go(s1.intervals, s2.intervals, List.empty).reverse)
  }

  /** @group Typeclass Instances */
  implicit val ScheduleShow: Show[Schedule] =
    Show.fromToString

  /** @group Typeclass Instances */
  implicit val ScheduleEqual: Eq[Schedule] = Eq.fromUniversalEquals

  /** @group Typeclass Instances */
  object UnionMonoid extends Monoid[Schedule] {
    def empty: Schedule = Schedule.Never

    def combine(x: Schedule, y: Schedule): Schedule = x.union(y)
  }

  /** @group Typeclass Instances */
  object IntersectionMonoid extends cats.Monoid[Schedule] {
    def empty: Schedule = Schedule.Always

    def combine(x: Schedule, y: Schedule): Schedule = x.intersection(y)
  }
}

trait ScheduleOptics { self: Schedule.type =>

  /** @group Optics */
  val intervals: Getter[Schedule, List[Interval]] =
    Getter(_.intervals)

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
      _.foldLeft(Schedule.Never)((s, i) => s.union(i)),
      _.intervals
    )
}
