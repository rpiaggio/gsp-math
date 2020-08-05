// Copyright (c) 2016-2020 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package gsp.math.skycalc.solver

import cats.implicits._
import gsp.math.skycalc.SkyCalcResults
import java.time.Instant
import java.time.Duration
import io.chrisdavenport.cats.time._

/**
  * Base trait for all calculators.
  * A calculator basically holds a matrix of values which are sampled at defined points in time over a given interval.
  * For each sampling point in time a vector with an arbitrary number of values can be stored.
  */
trait Calculator {

  val times: List[Instant]
  def toIndex(i: Instant): Int
  val values: List[SkyCalcResults]

  lazy val start: Instant = times.head
  lazy val end: Instant   = times.last
  lazy val samples: Int   = times.size

  /** True if the values for the given time are covered by this target. */
  def isDefinedAt(i:     Instant): Boolean                                  =
    i >= start && i <= end
  def value(ix:          Int): SkyCalcResults = values(ix)
  // def valueAt(t:         Long) = values(toIndex(t))
  def valueAt(field:     SkyCalcResults => Double, i: Instant): Double =
    field(values(toIndex(i)))
  def timedValues(field: SkyCalcResults => Double): List[(Instant, Double)] =
    times.zip(values.map(field))

  def min(field:  SkyCalcResults => Double): Double = field(values.minBy(field))
  def max(field:  SkyCalcResults => Double): Double = field(values.maxBy(field))
  def mean(field: SkyCalcResults => Double): Double = values.map(field).sum / samples
}

/**
  * Base trait for all calculators that provide a sampling of values over time at a given rate.
  * Define a single time to make this work.
  */
trait SingleValueCalculator extends Calculator {
  val time: Instant
  val times = List(time)
  def toIndex(i: Instant) = 0
}

/**
  * Base trait for all calculators that provide a sampling of values over time at a given rate.
  * Define an interval and a sampling rate to make this work.
  */
trait FixedRateCalculator extends Calculator {
  require(rate > Duration.ZERO)

  val defined: Interval
  val rate: Duration

  // the number of samples we need to have a sampling rate >= than expected
  private val cnt: Int            = math.ceil(defined.duration.toNanos.toDouble / rate.toNanos).toInt
  // the precise rate in milliseconds that corresponds to the expected rate
  private val preciseRate: Double = defined.duration.toMillis.toDouble / cnt

  /** Calculates a vector with times that cover the given interval. */
  val times: List[Instant] = {
    val ts = for {
      i <- 0 to cnt
    } yield Instant.ofEpochMilli(
      math.ceil(defined.start.toEpochMilli + i * preciseRate).toLong
    ) // always round up
    require(ts.head === defined.start)
    require(ts.last >= defined.end)
    List(ts: _*)
  }

  /** Gets the index to the left of the given value t. */
  def toIndex(i: Instant) = {
    require(i >= start)
    require(i <= end)
    val ix =
      math
        .floor(Interval(start, i).duration.toMillis / preciseRate)
        .toInt // always round down; the sample at this index gives a value <= t
    require(times(ix) <= i)
    require(ix == samples - 1 || times(ix + 1) > i)
    ix
  }

}

/**
  * Sampling at irregular intervals e.g middle dark time etc.
  * Define a vector with sampling times to make this work.
  */
trait IrregularIntervalCalculator extends Calculator {
  require(times.size > 0)

  /** Irregular interval calculators need to define a vector of times at which to sample the data. */
  val times: List[Instant]

  /** Gets the index to the left of the given value t. */
  def toIndex(i: Instant) = {
    require(i >= start)
    require(i <= end)
    val ix = times.zipWithIndex.reverse.dropWhile(_._1 > i).head._2
    // postconditions: useful for debugging / documentation
    // require(ix >= 0 && ix < samples)
    // require(times(ix) <= t && (ix == samples-1 || times(ix+1) > t))
    ix
  }
}

/**
  * Add capabilities for linear interpolation for times that fall between two calculated values.
  */
trait LinearInterpolatingCalculator extends Calculator {

  /**
    * Gets the value at instant i. If t falls between two values a linear approximation for the value is calculated
    * from the values to the left and to the right.
    */
  override def valueAt(field: SkyCalcResults => Double, i: Instant): Double = {
    val ix = toIndex(i)
    val i0 = times(ix)
    val v0 = field(values(ix))
    if (i0 === i || ix === samples - 1) v0
    else {
      val i1 = times(ix + 1)
      // require(t0 <= t && t < t1)
      val v1 = field(values(ix + 1))
      val v  =
        v0 + (i.toEpochMilli - i0.toEpochMilli).toDouble / (i1.toEpochMilli - i0.toEpochMilli) * (v1 - v0)
      // require((v0 >= v1 && v0 >= v && v >= v1) || (v0 < v1 && v0 <= v && v <= v1))
      v
    }
  }

}
