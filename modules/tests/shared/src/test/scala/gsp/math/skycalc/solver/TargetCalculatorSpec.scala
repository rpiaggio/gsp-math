package gsp.math.skycalc.solver

import cats.tests.CatsSuite
import java.time.ZonedDateTime
import java.time.LocalDate
import java.time.LocalTime
import gsp.math.Coordinates
import java.time.Instant
import gsp.math.Declination
import gsp.math.RightAscension
import org.scalactic.Tolerance

/**
  * Compare some random values with results from http://catserver.ing.iac.es/staralt/index.php
  * This is not meant to test the underlying SkyCalc implementations, we assume that this is all working,
  * this only tests the general mechanics of the TargetCalculator class.
  */
class TargetCalculatorSpec extends CatsSuite with Tolerance {
  import gsp.math.skycalc.GN

  test("Calculates Target") {
    val i      = ZonedDateTime.of(LocalDate.of(2014, 3, 1), LocalTime.of(20, 0, 0), GN.zoneId).toInstant
    val c      = (_: Instant) =>
      Coordinates(RightAscension.fromDoubleDegrees(150), Declination.fromDoubleDegrees(20).get)
    val target = TargetCalculator(GN, c, i)

    // check definition interval
    assert(!target.isDefinedAt(i.minusMillis(1)))
    assert(target.isDefinedAt(i))
    assert(!target.isDefinedAt(i.plusMillis(1)))

    // check some values
    assert(37.0 === target.elevation +- 1)
    assert(1.6 === target.airmass +- 0.1)
  }

  test("Calculates Target Interval") {
    val i                        = ZonedDateTime.of(LocalDate.of(2014, 3, 1), LocalTime.of(20, 0, 0), GN.zoneId).toInstant
    val c                        = (_: Instant) =>
      Coordinates(RightAscension.fromDoubleDegrees(150), Declination.fromDoubleDegrees(20).get)
    val interval                 = Interval(i, i.plusSeconds(4 * 60 * 60))
    val target: TargetCalculator = TargetCalculator(GN, c, interval)

    // check definition interval
    assert(!target.isDefinedAt(interval.start.minusMillis(1)))
    assert(target.isDefinedAt(interval.start))
    assert(target.isDefinedAt(interval.end))

    // check some values
    assert(37.0 === target.elevationAt(i) +- 1)
    assert(1.6 === target.airmassAt(i) +- 0.1)
    assert(89.0 === target.maxElevation +- 1)
    assert(37.0 === target.minElevation +- 1)
  }
}
