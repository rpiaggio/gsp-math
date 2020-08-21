// Copyright (c) 2016-2020 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package gsp.math.skycalc.solver

import cats.implicits._
import java.time.Instant
import gsp.math.Declination
import gsp.math.Angle
import gsp.math.HourAngle
import io.chrisdavenport.cats.time._
import spire.math.Number
import spire.math.Rational
import java.time.Duration
import gsp.math.optics.Spire._

object implicits {

  implicit def closestGetter[A]: CalcGetter[GetterStrategy.Closest, A] =
    new CalcGetter[GetterStrategy.Closest, A] {
      def get[T](
        calc:  Calculator[GetterStrategy.Closest, T]
      )(field: T => A)(instant: Instant): A = {
        val idx         = calc.toIndex(instant)
        val timedResult = calc.timedResults(idx)
        if (idx >= calc.instants.length - 1)
          field(timedResult._2)
        else {
          val nextTimedResult = calc.timedResults(idx + 1)
          if (
            Duration.between(timedResult._1, instant) <
              Duration.between(instant, nextTimedResult._1)
          )
            field(timedResult._2)
          else
            field(nextTimedResult._2)
        }
      }
    }

  implicit val interpolatedNumberGetter: CalcGetter[GetterStrategy.LinearInterpolating, Number] =
    new CalcGetter[GetterStrategy.LinearInterpolating, Number] {
      def get[T](
        calc:  Calculator[GetterStrategy.LinearInterpolating, T]
      )(field: T => Number)(instant: Instant): Number = {
        val idx     = calc.toIndex(instant)
        val result0 = calc.timedResults(idx)
        val i0      = result0._1
        val v0      = field(result0._2)
        if (i0 === instant || idx === calc.timedResults.length - 1) v0
        else {
          val result1 = calc.timedResults(idx + 1)
          val i1      = result1._1
          // require(t0 <= t && t < t1)
          val v1      = field(result1._2)
          val v       =
            v0 + Rational(instant.toEpochMilli - i0.toEpochMilli,
                          i1.toEpochMilli - i0.toEpochMilli
            ) * (v1 - v0)
          // require((v0 >= v1 && v0 >= v && v >= v1) || (v0 < v1 && v0 <= v && v <= v1))
          v
        }
      }
    }

  // Fails on Infinity
  implicit val interpolatedDoubleGetter: CalcGetter[GetterStrategy.LinearInterpolating, Double] =
    interpolatedNumberGetter.imap(n => numberDouble.get(n.some).get)(d =>
      numberDouble.reverseGet(d.some).get
    )

  // Fails on Infinity
  implicit val interpolatedFloatGetter: CalcGetter[GetterStrategy.LinearInterpolating, Float] =
    interpolatedNumberGetter.imap(n => numberFloat.get(n.some).get)(d =>
      numberFloat.reverseGet(d.some).get
    )

  implicit val interpolatedLongGetter: CalcGetter[GetterStrategy.LinearInterpolating, Long] =
    interpolatedNumberGetter.imap(numberLong)

  implicit val interpolatedIntGetter: CalcGetter[GetterStrategy.LinearInterpolating, Int] =
    interpolatedNumberGetter.imap(numberInt)

  implicit val interpolatedAngleGetter: CalcGetter[GetterStrategy.LinearInterpolating, Angle] =
    interpolatedLongGetter.imap(Angle.microarcseconds.reverse)

  implicit val interpolatedDeclinationGetter
    : CalcGetter[GetterStrategy.LinearInterpolating, Declination] =
    interpolatedAngleGetter.imap((Declination.fromAngleWithCarry _).andThen(_._1))(_.toAngle)

  implicit val interpolatedHourAngleGetter
    : CalcGetter[GetterStrategy.LinearInterpolating, HourAngle] =
    interpolatedAngleGetter.imap(HourAngle.angle.reverse)

  implicit def toTupledGetter[G, A, B](implicit
    getterA: CalcGetter[G, A],
    getterB: CalcGetter[G, B]
  ): CalcGetter[G, (A, B)] =
    new CalcGetter[G, (A, B)] {
      def get[T](calc: Calculator[G, T])(field: T => (A, B))(instant: Instant): (A, B) =
        (
          getterA.get(calc)(field.andThen(_._1))(instant),
          getterB.get(calc)(field.andThen(_._2))(instant)
        )
    }
}
