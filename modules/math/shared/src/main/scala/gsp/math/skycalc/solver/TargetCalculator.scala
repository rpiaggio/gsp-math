// Copyright (c) 2016-2020 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package gsp.math.skycalc.solver

import gsp.math.Coordinates
import gsp.math.Place
import gsp.math.skycalc.ImprovedSkyCalc

import java.time.Duration
import java.time.Instant
import gsp.math.skycalc.SkyCalcResults

/**
  * Target calculator that allows to calculate different attributes of a target for a given interval at a given sampling
  * rate.
  *
  * It caches the values for a target for a given interval and sampling rate; this is relevant for places
  *       where these values are needed repetitively because the calculation is pretty complex and slow.
  *
  * If in doubt use {@link isDefinedAt} to make sure that values for a given time are actually calculated before
  * accessing them, otherwise an out of bounds exception will be thrown.
  */
trait TargetCalculator extends Calculator {
  val place: Place
  val targetLocation: Instant => Coordinates

  val values: List[SkyCalcResults] = calculate()

  // ==  Gets the first of all calculated values for a given field, use this if only one value was calculated. ==
  lazy val elevation: Double        = valueAt(_.altitudeRaw, start)
  lazy val azimuth: Double          = valueAt(_.azimuthRaw, start)
  lazy val airmass: Double          = valueAt(_.airmass, start)
  lazy val lunarDistance: Double    = valueAt(_.lunarDistance, start)
  lazy val parallacticAngle: Double = valueAt(_.parallacticAngleRaw, start)
  lazy val hourAngle: Double        = valueAt(_.hourAngleRaw, start)
  lazy val skyBrightness: Double    = valueAt(_.totalSkyBrightness, start)

  // ==  Accessors for any calculated values for a given field, use this if an interval of values was sampled. ==
  def elevationAt(i: Instant): Double = valueAt(_.altitudeRaw, i)
  lazy val minElevation: Double  = min(_.altitudeRaw)
  lazy val maxElevation: Double  = max(_.altitudeRaw)
  lazy val meanElevation: Double = mean(_.altitudeRaw)

  def azimuthAt(i: Instant): Double = valueAt(_.azimuthRaw, i)
  lazy val minAzimuth: Double  = min(_.azimuthRaw)
  lazy val maxAzimuth: Double  = max(_.azimuthRaw)
  lazy val meanAzimuth: Double = mean(_.azimuthRaw)

  def airmassAt(i: Instant): Double = valueAt(_.airmass, i)
  lazy val minAirmass: Double  = min(_.airmass)
  lazy val maxAirmass: Double  = max(_.airmass)
  lazy val meanAirmass: Double = mean(_.airmass)

  def lunarDistanceAt(i: Instant): Double = valueAt(_.lunarDistance, i)
  lazy val minLunarDistance: Double  = min(_.lunarDistance)
  lazy val maxLunarDistance: Double  = max(_.lunarDistance)
  lazy val meanLunarDistance: Double = mean(_.lunarDistance)

  def parallacticAngleAt(i: Instant): Double = valueAt(_.parallacticAngleRaw, i)
  lazy val minParallacticAngle: Double  = min(_.parallacticAngleRaw)
  lazy val maxParallacticAngle: Double  = max(_.parallacticAngleRaw)
  lazy val meanParallacticAngle: Double = mean(_.parallacticAngleRaw)

  // If the target is visible during the scheduled time, return the weighted mean parallactic angle as Some(angle in degrees).
  // Otherwise, the target is not visible, so return None.
  lazy val weightedMeanParallacticAngle: Option[Double] = {
    val (weightedAngles, weights) = values
      .map(_.parallacticAngleRaw)
      .zip(times)
      .zip(values.map(_.airmass))
      .map {
        case ((angle, t), airmass) =>
          // Wrap negative angles as per Andy's comment in OCSADV-16.
          val normalizedAngle = {
            if (angle < 0) {
              val normalizingFactor = {
                val dec = targetLocation(t).dec.toAngle.toSignedDoubleDegrees
                if (dec - place.latitude.toAngle.toSignedDoubleDegrees < -10) 0
                else if (dec - place.latitude.toAngle.toSignedDoubleDegrees < 10) 180
                else 360
              }
              angle + normalizingFactor
            } else angle
          }

          //val weight = if (airmass <= 1.0) 0.0 else 1.6 * math.pow(airmass - 1.0, 0.6)
          val weight = if (airmass <= 1.0) 0.0 else math.pow(airmass - 1.0, 1.3)
          (normalizedAngle * weight, weight)
      }
      .unzip

    val weightedSum = weights.sum
    if (weightedSum == 0) None
    else Some(weightedAngles.sum / weightedSum)
  }

  def hourAngleAt(i: Instant): Double = valueAt(_.hourAngleRaw, i)
  lazy val minHourAngle: Double   = min(_.hourAngleRaw)
  lazy val maxHoursAngle: Double  = max(_.hourAngleRaw)
  lazy val meanHoursAngle: Double = mean(_.hourAngleRaw)

  def skyBrightnessAt(i: Instant): Double = valueAt(_.totalSkyBrightness, i)
  lazy val minSkyBrightness: Double  = min(_.totalSkyBrightness)
  lazy val maxSkyBrightness: Double  = max(_.totalSkyBrightness)
  lazy val meanSkyBrightness: Double = mean(_.totalSkyBrightness)

  /**
    * Calculates all values for the given times.
    * @return
    */
  protected def calculate(): List[SkyCalcResults] = {
    val skycalc = ImprovedSkyCalc(place)

    times.map(i => skycalc.calculate(targetLocation(i), i, true))
  }
}

case class IntervalTargetCalculator(
  place:          Place,
  targetLocation: Instant => Coordinates,
  defined:        Interval,
  rate:           Duration
) extends FixedRateCalculator
    with LinearInterpolatingCalculator
    with TargetCalculator

case class SampleTargetCalculator(
  place:          Place,
  targetLocation: Instant => Coordinates,
  times:          List[Instant]
) extends IrregularIntervalCalculator
    with LinearInterpolatingCalculator
    with TargetCalculator

case class SingleValueTargetCalculator(
  place:          Place,
  targetLocation: Instant => Coordinates,
  time:           Instant
) extends SingleValueCalculator
    with TargetCalculator

object TargetCalculator {

  def apply(
    place:          Place,
    targetLocation: Instant => Coordinates,
    defined:        Interval,
    rate:           Duration = Duration.ofSeconds(30)
  ) =
    new IntervalTargetCalculator(place, targetLocation, defined, rate)

  def apply(place: Place, targetLocation: Instant => Coordinates, time: Instant): TargetCalculator =
    new SingleValueTargetCalculator(place, targetLocation, time)

  def apply(
    place:          Place,
    targetLocation: Instant => Coordinates,
    times:          List[Instant]
  ): TargetCalculator =
    new SampleTargetCalculator(place, targetLocation, times)
}
