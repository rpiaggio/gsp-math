// Copyright (c) 2016-2020 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package gsp.math.skycalc

import cats.implicits._
import cats.Eq
import java.time.Duration
import java.time.Instant
import org.scalacheck.Gen
import org.scalacheck.Arbitrary._
import gsp.math.arb.ArbTime._
import io.chrisdavenport.cats.time._
import org.scalacheck.Gen.Choose

package object solver {
  private val MaxDelta: Long = Duration.ofMinutes(10).toNanos

  def interval(start: Int, end: Int): Interval =
    Interval.unsafe(Instant.ofEpochMilli(start.toLong), Instant.ofEpochMilli(end.toLong))

  implicit val chooseInstant: Choose[Instant] = new Choose[Instant] {
    def choose(min: Instant, max: Instant): Gen[Instant] =
      for {
        seconds <- Gen.choose(min.getEpochSecond, max.getEpochSecond)
        nanosMin = if (seconds === min.getEpochSecond) min.getNano.toLong else 0L
        nanosMax = if (seconds === max.getEpochSecond) max.getNano.toLong
                   else Constants.NanosPerSecond - 1
        nanos   <- Gen.choose(nanosMin, nanosMax)
      } yield Instant.ofEpochSecond(seconds, nanos)
  }

  def instantInInterval(
    interval:     Interval,
    includeStart: Boolean = true,
    includeEnd:   Boolean = false,
    specials:     List[Instant] = List.empty
  ): Gen[Instant] = {
    val basics            = List(Instant.MIN, Instant.MAX)
    val basicsAndSpecials =
      (basics ++ specials).filter(_ >= interval.start).filter(_ <= interval.end)
    val freqs             =
      basicsAndSpecials.map(v => (1, Gen.const(v))) :+ ((15 + basicsAndSpecials.length,
                                                         Gen
                                                           .choose(interval.start, interval.end)
                                                        )
      )

    Gen
      .frequency(freqs: _*)
      .suchThat(_ > interval.start || includeStart)
      .suchThat(_ < interval.end || includeEnd)
  }

  // There might not be instants outside the interval if the interval is (Instant.MIN, Instant.MAX).
  def instantOutsideInterval(
    interval:     Interval,
    includeStart: Boolean = false,
    includeEnd:   Boolean = true
  ): Gen[Option[Instant]] =
    Gen.oneOf(
      Interval(Instant.MIN, interval.start).fold(
        Gen.const(if (includeStart) interval.start.some else none)
      )(before => Gen.some(instantInInterval(before, includeEnd = includeStart))),
      Interval(interval.end, Instant.MAX)
        .fold(Gen.const(if (includeEnd) interval.end.some else none))(after =>
          Gen.some(instantInInterval(after, includeStart = includeEnd))
        )
    )

  def rateForInterval(interval: Interval): Gen[Duration] =
    for {
      samples <- Gen.choose(50L, 400L)
      delta   <- Gen.choose(0, MaxDelta)
    } yield interval.duration.dividedBy(samples).plusNanos(delta)

  def distinctZip[A: Eq](gen1: Gen[A], gen2: Gen[A]): Gen[(A, A)] =
    Gen.zip(gen1, gen2).suchThat(t => t._1 =!= t._2)

  def instantWithSpecialInterval(interval: Interval): Gen[Instant] =
    Gen.frequency((1, Gen.const(interval.start)),
                  (1, Gen.const(interval.end)),
                  (18, arbitrary[Instant])
    )

  def untilEndOfInterval(interval: Interval, includeEnd: Boolean = false): Gen[Instant] =
    instantInInterval(Interval.unsafe(Instant.MIN, interval.end),
                      includeEnd,
                      specials = List(interval.start, interval.end)
    )

  def fromStartOfInterval(interval: Interval, includeStart: Boolean = true): Gen[Instant] =
    instantInInterval(Interval.unsafe(interval.start, Instant.MAX),
                      includeStart,
                      specials = List(interval.start, interval.end)
    )

}
