// Copyright (c) 2016-2020 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package gsp.math.skycalc

import cats.implicits._
import java.time.Duration
import java.time.Instant
import org.scalacheck.Gen
import io.chrisdavenport.cats.time._

package object solver {
  private val MaxDelta: Long = Duration.ofMinutes(10).toNanos

  def interval(start: Int, end: Int): Interval =
    Interval.unsafe(Instant.ofEpochMilli(start.toLong), Instant.ofEpochMilli(end.toLong))

  def instantInInterval(interval: Interval, includeEnd: Boolean = false): Gen[Instant] =
    (for {
      seconds <- Gen.chooseNum(interval.start.getEpochSecond, interval.end.getEpochSecond)
      nanosMin =
        if (seconds === interval.start.getEpochSecond) interval.start.getNano.toLong else 0L
      nanosMax = if (seconds === interval.end.getEpochSecond) interval.end.getNano.toLong
                 else Constants.NanosPerSecond - 1
      nanos   <- Gen.chooseNum(nanosMin, nanosMax)
    } yield Instant.ofEpochSecond(seconds, nanos))
      .suchThat(_ < interval.end || includeEnd)

  def instantOutsideInterval(interval: Interval): Gen[Instant] =
    Gen.oneOf(instantInInterval(Interval.unsafe(Instant.MIN, interval.start)),
              instantInInterval(Interval.unsafe(interval.end, Instant.MAX), includeEnd = true)
    )

  def rateForInterval(interval: Interval): Gen[Duration] =
    for {
      samples <- Gen.choose(50L, 400L)
      delta   <- Gen.choose(0, MaxDelta)
    } yield interval.duration.dividedBy(samples).plusNanos(delta)
}
