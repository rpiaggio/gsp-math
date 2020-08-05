// Copyright (c) 2016-2020 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package gsp.math.arb

import gsp.math.skycalc.solver.Interval
import org.scalacheck._
import org.scalacheck.Arbitrary._
import java.time.Instant

trait ArbInterval {
  import ArbTime._

  implicit val arbInterval: Arbitrary[Interval] =
    Arbitrary(
      for {
        a <- arbitrary[Instant]
        b <- arbitrary[Instant]
      } yield {
        val ab = List(a.toEpochMilli, b.toEpochMilli).sorted
        Interval(ab(0), ab(1))
      }
    )
}

object ArbInterval extends ArbInterval
