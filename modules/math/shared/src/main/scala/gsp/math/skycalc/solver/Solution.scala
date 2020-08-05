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

/**
  * Representation of a solution for a constraint defined by an arbitrary number of intervals.
  * The intervals are sorted by their start time and don't overlap or abut, i.e. the solution is always represented
  * by the smallest possible set of intervals.
  */
case class Solution(intervals: List[Interval]) {

  /** True if the solution (i.e. any of its intervals) contains instant i. */
  def contains(i: Instant) = intervals.exists(_.contains(i))

  def never: Boolean            = intervals.size == 0
  def earliest: Option[Instant] = intervals.headOption.map(_.start)
  def latest: Option[Instant]   = intervals.lastOption.map(_.end)
  def duration: Duration        = intervals.foldMap(_.duration)

  /**
    * Adds a solution to this solution.
    * @param other
    * @return
    */
  def add(other: Solution): Solution =
    Solution(addIntervals(intervals, other.intervals))

  /**
    * Adds an interval to this solution.
    * @param interval
    * @return
    */
  def add(interval: Interval): Solution =
    Solution(addIntervals(intervals, List(interval)))

  /**
    * True if any part of this solution overlaps with the given interval.
    * @param interval
    * @return
    */
  def overlaps(interval: Interval): Boolean =
    intervals.exists(i => i.overlaps(interval))

  /**
    * Restricts a solution to only the intervals in the given interval.
    * @param interval
    * @return
    */
  def restrictTo(interval: Interval): Solution =
    Solution(
      intervals
        .filter(i => i.end >= interval.start && i.start <= interval.end)
        .map(i => Interval(i.start.max(interval.start), i.end.min(interval.end)))
    )

  // def allDay(localTime: TimeZone): Solution = {
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

  //   if (this == Solution.Always)
  //     Solution.Always // Always can not be "blown" up, don't try
  //   else
  //     Solution(removeDuplicates(Seq(), intervals.map(blowUp)))

  // }

  /**
    * Combines two solutions.
    * Merges all overlapping and abutting intervals.
    */
  def combine(s: Solution): Solution = Solution(Interval.combine(intervals, s.intervals))

  /**
    * Combines this solution with a sequence of ordered intervals.
    * Merges all overlapping and abutting intervals.
    * @param otherIntervals
    * @return
    */
  def combine(otherIntervals: List[Interval]): Solution =
    Solution(Interval.combine(intervals, otherIntervals))

  /**
    * Intersects a solution with another one.
    * The result will contain all intervals of this solution which are covered by both solutions.
    */
  def intersect(s: Solution): Solution = Solution(Interval.intersect(intervals, s.intervals))

  /**
    * Reduce a solution by another one.
    * The result will contain all intervals of this solution which are NOT covered by the the given solution.
    * @param s
    * @return
    */
  def reduce(s: Solution): Solution = Solution(Interval.reduce(intervals, s.intervals))

  def reduce(otherIntervals: List[Interval]): Solution =
    Solution(Interval.reduce(intervals, otherIntervals))

  // ===== helpers

  private def addIntervals(i1: List[Interval], i2: List[Interval]): List[Interval] =
    if (i1.isEmpty && i2.isEmpty) List.empty
    else if (!i1.isEmpty && i2.isEmpty) i1
    else if (i1.isEmpty && !i2.isEmpty) i2
    else if (i1.last.abuts(i2.head)) (i1.dropRight(1) :+ i1.last.plus(i2.head)) ++ i2.tail
    else i1 ++ i2

}

/**
  * Companion objects with convenience constructors.
  */
object Solution extends SolutionOptics {

  /** Solution that is always true (i.e. for any time t). */
  val Always = new Solution(List(Interval(Instant.MIN, Instant.MAX)))

  /** Solution that is never true. */
  val Never = Solution()

  /** Convenience constructors. */
  def apply(): Solution = new Solution(List.empty)
  def apply(start:    Instant, end: Instant) = new Solution(List(Interval(start, end)))
  def apply(interval: Interval): Solution = apply(List(interval))

  /** @group Typeclass Instances */
  implicit val SolutionShow: Show[Solution] =
    Show.fromToString

  /** @group Typeclass Instances */
  implicit val SolutionEqual: Eq[Solution] = Eq.fromUniversalEquals

  /** @group Typeclass Instances */
  implicit object SolutionMonoid extends Monoid[Solution] {
    def empty: Solution = Solution.Never

    def combine(x: Solution, y: Solution): Solution = x.combine(y)
  }
}

trait SolutionOptics { self: Solution.type =>

  /** @group Optics */
  val intervals: Getter[Solution, List[Interval]] =
    Getter(_.intervals)
}
