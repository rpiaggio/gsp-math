// Copyright (c) 2016-2020 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package gsp.math.skycalc.solver

import cats.implicits._
import java.time.Instant
import gsp.math.Declination
import io.chrisdavenport.cats.time._
import gsp.math.Angle

trait LowPriorityImplicits {
  implicit def toTupledValueGetter[S, A, B](implicit
    getterA: ResultValueGetter[S, A],
    getterB: ResultValueGetter[S, B]
  ): ResultValueGetter[S, (A, B)] =
    new ResultValueGetter[S, (A, B)] {
      def get[Results](
        timedResults: List[(Instant, Results)],
        toIndex:      Instant => Int,
        field:        Results => (A, B),
        instant:      Instant
      ): (A, B) =
        (getterA.get(timedResults, toIndex, field.andThen(_._1), instant),
         getterB.get(timedResults, toIndex, field.andThen(_._2), instant)
        )
    }
}

trait MidPriorityImplicits extends LowPriorityImplicits {
  implicit def fromTupledSecondValueGetter[S, A, B](implicit
    getter: ResultValueGetter[S, (A, B)]
  ): ResultValueGetter[S, B] =
    new ResultValueGetter[S, B] {
      def get[Results](
        timedResults: List[(Instant, Results)],
        toIndex:      Instant => Int,
        field:        Results => B,
        instant:      Instant
      ): B =
        getter.get(timedResults, toIndex, field.andThen(b => (null.asInstanceOf[A], b)), instant)._2
    }
}

object implicits extends MidPriorityImplicits {
  implicit def exactValueGetter[A]: ResultValueGetter[GetterStrategy.Exact, A] =
    new ResultValueGetter[GetterStrategy.Exact, A] {
      def get[Results](
        timedResults: List[(Instant, Results)],
        toIndex:      Instant => Int,
        field:        Results => A,
        instant:      Instant
      ): A =
        field(timedResults(toIndex(instant))._2)
    }

  implicit val interpolatedDoubleValueGetter
    : ResultValueGetter[GetterStrategy.LinearInterpolating, Double] =
    new ResultValueGetter[GetterStrategy.LinearInterpolating, Double] {
      def get[Results](
        timedResults: List[(Instant, Results)],
        toIndex:      Instant => Int,
        field:        Results => Double,
        instant:      Instant
      ): Double = {
        val idx     = toIndex(instant)
        val result0 = timedResults(idx)
        val i0      = result0._1
        val v0      = field(result0._2)
        if (i0 === instant || idx === timedResults.length - 1) v0
        else {
          val result1 = timedResults(idx + 1)
          val i1      = result1._1
          // require(t0 <= t && t < t1)
          val v1      = field(result1._2)
          val v       =
            v0 + (instant.toEpochMilli - i0.toEpochMilli).toDouble / (i1.toEpochMilli - i0.toEpochMilli) * (v1 - v0)
          // require((v0 >= v1 && v0 >= v && v >= v1) || (v0 < v1 && v0 <= v && v <= v1))
          v
        }
      }
    }

  implicit val interpolatedDeclinationValueGetter
    : ResultValueGetter[GetterStrategy.LinearInterpolating, Declination] =
    new ResultValueGetter[GetterStrategy.LinearInterpolating, Declination] {
      def get[Results](
        timedResults: List[(Instant, Results)],
        toIndex:      Instant => Int,
        field:        Results => Declination,
        instant:      Instant
      ): Declination =
        Declination
          .fromAngleWithCarry(
            Angle.fromDoubleDegrees(
              interpolatedDoubleValueGetter
                .get(timedResults, toIndex, field.andThen(_.toAngle.toSignedDoubleDegrees), instant)
            )
          )
          ._1
    }

  implicit def fromTupledFirstValueGetter[S, A, B](implicit
    getter: ResultValueGetter[S, (A, B)]
  ): ResultValueGetter[S, A] =
    new ResultValueGetter[S, A] {
      def get[Results](
        timedResults: List[(Instant, Results)],
        toIndex:      Instant => Int,
        field:        Results => A,
        instant:      Instant
      ): A =
        getter.get(timedResults, toIndex, field.andThen(a => (a, null.asInstanceOf[B])), instant)._1
    }
}
