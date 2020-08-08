// Copyright (c) 2016-2020 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package gsp.math.skycalc.solver

import cats.implicits._
import java.time.Instant
import java.time.Duration
import io.chrisdavenport.cats.time._

object GetterStrategy {
  sealed trait Exact
  sealed trait LinearInterpolating
}

trait ResultValueGetter[G, A] {
  def get[Results](
    timedResults: List[(Instant, Results)],
    toIndex:      Instant => Int,
    field:        Results => A,
    instant:      Instant
  ): A
}

/**
  * Base trait for all calculators.
  * A calculator basically holds a list of results which are sampled at defined points in time over a given interval.
  */
trait Calculator[R, G] { // Results, GetterStrategy
  val instants: List[Instant]
  def toIndex(i: Instant): Int
  val result: Instant => R

  lazy val start: Instant   = instants.head
  lazy val end: Instant     = instants.last
  lazy val sampleCount: Int = instants.size

  lazy val results: List[R]                 = instants.map(result)
  lazy val timedResults: List[(Instant, R)] = instants.zip(results)

  /** True if the values for the given time are covered by this target. */
  def isDefinedAt(i: Instant): Boolean =
    i >= start && i <= end

  def result(ix: Int): R = results(ix)

  def valueAt[A](field: R => A, i: Instant)(implicit
    getter:             ResultValueGetter[G, A]
  ): A =
    getter.get(timedResults, toIndex, field, i)

  def timedValues[A](field:   R => A): List[(Instant, A)] =
    timedResults.map { case (i, r) => (i, field(r)) }

  def min[A: Ordering](field: R => A): A                  = field(results.minBy(field))
  def max[A: Ordering](field: R => A): A                  = field(results.maxBy(field))
  def mean(field:             R => Double): Double        = results.map(field).sum / sampleCount
}

trait SingleValueCalculator[R] extends Calculator[R, GetterStrategy.Exact] {
  import implicits.exactValueGetter

  val instant: Instant
  val instants = List(instant)
  def toIndex(i: Instant) = 0

  def value[A](field: R => A): A =
    valueAt(field, start)
}

trait FixedRateCalculator[R] extends Calculator[R, GetterStrategy.LinearInterpolating] {
  require(rate > Duration.ZERO)

  val interval: Interval
  val rate: Duration

  private val maxEnd: Instant = interval.end.plus(rate)

  /** Calculates a vector with times that cover the given interval. */
  val instants: List[Instant] =
    List.unfold(interval.start) {
      case i if i < maxEnd => (i, i.plus(rate)).some
      case _               => none
    }

  /** Gets the index to the left of the given value t. */
  def toIndex(i: Instant): Int = {
    require(i >= start)
    require(i <= end)
    (Duration.between(start, i).toNanos / rate.toNanos).toInt
  }
}

trait IrregularIntervalCalculator[R] extends Calculator[R, GetterStrategy.LinearInterpolating] {
  require(instants.size > 0)

  /** Gets the index to the left of the given value t. */
  def toIndex(i: Instant) = {
    require(i >= start)
    require(i <= end)
    val ix = instants.zipWithIndex.reverse.dropWhile(_._1 > i).head._2
    // postconditions: useful for debugging / documentation
    // require(ix >= 0 && ix < samples)
    // require(times(ix) <= t && (ix == samples-1 || times(ix+1) > t))
    ix
  }
}
