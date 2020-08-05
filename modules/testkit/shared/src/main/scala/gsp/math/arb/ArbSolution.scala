// Copyright (c) 2016-2020 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package gsp.math.arb

import cats.implicits._
import gsp.math.skycalc.solver.Interval
import gsp.math.skycalc.solver.Solution
import org.scalacheck._
import org.scalacheck.Arbitrary._
import java.time.Instant

trait ArbSolution {
  import ArbTime._

  implicit val arbSolution: Arbitrary[Solution] =
    Arbitrary {
      arbitrary[List[Instant]]
        .suchThat(list => list.length === list.distinct.length)
        .map { list =>
          Solution(
            list.sorted
              .grouped(2)
              .collect {
                case List(a, b) => Interval(a.toEpochMilli, b.toEpochMilli)
              }
              .toList
          )
        }
    }
}

object ArbSolution extends ArbSolution
