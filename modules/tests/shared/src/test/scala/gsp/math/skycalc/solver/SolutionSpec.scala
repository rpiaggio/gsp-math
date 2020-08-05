// Copyright (c) 2016-2020 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package gsp.math.skycalc.solver

import cats.tests.CatsSuite

import cats.Eq
import cats.Monoid
import cats.kernel.laws.discipline.MonoidTests
import gsp.math.arb.ArbSolution._

final class SolutionSpec extends CatsSuite {

  implicit val SolutionEqual: Eq[Solution] = Eq.fromUniversalEquals

  implicit object SolutionMonoid extends Monoid[Solution] {
    def empty: Solution = Solution.Never

    def combine(x: Solution, y: Solution): Solution = x.combine(y)
  }

  checkAll("Monoid", MonoidTests[Solution].monoid)
}
