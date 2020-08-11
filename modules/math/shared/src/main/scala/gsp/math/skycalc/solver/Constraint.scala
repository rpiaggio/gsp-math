// Copyright (c) 2016-2020 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package gsp.math.skycalc.solver

import implicits._
import cats.implicits._
import java.time.Duration
import java.time.Instant
import gsp.math.Declination
import gsp.math.HourAngle
import gsp.math.skycalc.SkyCalcResults

// import edu.gemini.util.skycalc.Night

trait Constraint[R, A] {
  protected val solver: Solver[A]

  // def solve(nights: Seq[Night], param: A): Solution =
  //   nights.map(n => solve(n, param)).foldLeft(Solution.Never)(_ add _)

  // def solve(night: Night, param: A): Solution =
  //   solve(night.interval, param)

  /** Finds solution for an interval. */
  def solve[G](interval: Interval, calc: Calculator[R, G])(implicit
    getter:              ResultValueGetter[G, A]
  ): Schedule =
    solver.solve(this, interval, calc)

  /** This function defines the actual constraint by returning true or false for a given instant <code>i</code>
    * depending on whether to constraint is met or not.
    */
  def metAt[G](i: Instant, calc: Calculator[R, G])(implicit
    getter:       ResultValueGetter[G, A]
  ): Boolean

}

/**
  * Implementation for an elevation constraint that uses the pre-calculated data from a
  * {@see edu.gemini.util.skycalc.calc.TargetCalc} object.
  */
case class ElevationConstraint(
  min:       Declination,
  max:       Declination,
  tolerance: Duration = Duration.ofSeconds(30)
) extends Constraint[SkyCalcResults, Declination] {
  protected val solver = DefaultSolver[Declination](tolerance)
  override def metAt[G](i: Instant, calc: Calculator[SkyCalcResults, G])(implicit
    getter:                ResultValueGetter[G, Declination]
  ): Boolean = {
    val elevation = calc.valueAt(_.altitude, i)
    elevation >= min && elevation <= max
  }
}

// case class MoonElevationConstraint(
//   min:       Double,
//   max:       Double,
//   tolerance: Long = Duration.ofSeconds(30).toMillis
// ) extends Constraint[MoonCalculator] {
//   protected val solver = DefaultSolver[MoonCalculator](tolerance)
//   def metAt(t: Long, moon: MoonCalculator): Boolean = {
//     val elevation = moon.elevationAt(t)
//     elevation >= min && elevation <= max
//   }
// }

case class SkyBrightnessConstraint(
  min:       Double,
  max:       Double,
  tolerance: Duration = Duration.ofSeconds(30)
) extends Constraint[SkyCalcResults, Double] {
  protected val solver = DefaultSolver[Double](tolerance)
  override def metAt[G](i: Instant, calc: Calculator[SkyCalcResults, G])(implicit
    getter:                ResultValueGetter[G, Double]
  ): Boolean = {
    val skyBrightness = calc.valueAt(_.totalSkyBrightness, i)
    skyBrightness >= min && skyBrightness <= max
  }
}

case class AirmassConstraint(
  min:       Double,
  max:       Double,
  tolerance: Duration = Duration.ofSeconds(30)
) extends Constraint[SkyCalcResults, (Double, Declination)] {
  protected val solver = DefaultSolver[(Double, Declination)](tolerance)
  override def metAt[G](i: Instant, calc: Calculator[SkyCalcResults, G])(implicit
    getter:                ResultValueGetter[G, (Double, Declination)]
  ): Boolean = {
    val airmass = calc.valueAt(_.airmass, i)
    // NOTE: we need to work around errors with interpolation etc which may cause to give wrong airmass values for very small altitudes (<1deg)
    calc
      .valueAt(_.altitude, i)
      .toAngle
      .toDoubleDegrees >= 5 && airmass >= min && airmass <= max
  }
}

case class HourAngleConstraint(
  min:       HourAngle,
  max:       HourAngle,
  tolerance: Duration = Duration.ofSeconds(30)
) extends Constraint[SkyCalcResults, (HourAngle, Declination)] {
  protected val solver = DefaultSolver[(HourAngle, Declination)](tolerance)
  override def metAt[G](i: Instant, calc: Calculator[SkyCalcResults, G])(implicit
    getter:                ResultValueGetter[G, (HourAngle, Declination)]
  ): Boolean = {
    val hourAngle = calc.valueAt(_.hourAngle, i)
    // NOTE: we need to work around errors with interpolation etc which may cause to give wrong hour angle values for very small altitudes (<1deg)
    calc
      .valueAt(_.altitude, i)
      .toAngle
      .toSignedDoubleDegrees >= 5 && hourAngle.toMicroarcseconds >= min.toMicroarcseconds && hourAngle.toMicroarcseconds <= max.toMicroarcseconds
  }
}
