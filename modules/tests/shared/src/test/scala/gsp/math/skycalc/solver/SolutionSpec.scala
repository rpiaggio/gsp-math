// Copyright (c) 2016-2020 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package gsp.math.skycalc.solver

import cats.tests.CatsSuite

import cats.Eq
import cats.Show
import cats.kernel.laws.discipline.MonoidTests
import gsp.math.arb._

final class SolutionSpec extends CatsSuite {
  import ArbSolution._

  test("Equality must be natural") {
    forAll { (a: Solution, b: Solution) =>
      a.equals(b) shouldEqual Eq[Solution].eqv(a, b)
    }
  }

  test("Show must be natural") {
    forAll { (a: Solution) =>
      a.toString shouldEqual Show[Solution].show(a)
    }
  }

  checkAll("Monoid", MonoidTests[Solution].monoid)

  test("Add") {
    val s1 = Solution(interval(1, 2))
    val s2 = Solution(interval(2, 5))
    val s3 = Solution(interval(8, 9))

    assert(Solution(interval(1, 5)) === s1.add(s2))
    assert(Solution(List(interval(1, 5), interval(8, 9))) === s1.add(s2).add(s3))
  }

  test("Combine") {
    val s1     = Solution(List(interval(1, 2), interval(5, 9)))
    val s2     = Solution(List(interval(2, 3), interval(4, 8), interval(11, 14)))
    val result = Solution(List(interval(1, 3), interval(4, 9), interval(11, 14)))

    assert(result === s1.combine(s2))
    assert(result === s2.combine(s1))
  }

  test("Intersect") {
    val s1     = Solution(List(interval(1, 2), interval(5, 9)))
    val s2     = Solution(List(interval(2, 3), interval(4, 8), interval(11, 14)))
    val result = Solution(interval(5, 8))

    assert(result === s1.intersect(s2))
    assert(result === s2.intersect(s1))
  }

  test("Simple Reduce") {
    val s = Solution(List(interval(10, 20)))

    val s1 = Solution()
    val s2 = Solution(List(interval(5, 6)))
    val s3 = Solution(List(interval(5, 15)))
    val s4 = Solution(List(interval(12, 18)))
    val s5 = Solution(List(interval(15, 25)))
    val s6 = Solution(List(interval(25, 30)))
    val s7 = Solution(List(interval(5, 25)))

    assert(s === s.reduce(s1))
    assert(s === s.reduce(s2))
    assert(Solution(List(interval(15, 20))) === s.reduce(s3))
    assert(Solution(List(interval(10, 12), interval(18, 20))) === s.reduce(s4))
    assert(Solution(List(interval(10, 15))) === s.reduce(s5))
    assert(s === s.reduce(s6))
    assert(Solution() === s.reduce(s7))
  }

  test("Empty Reduce") {
    val empty = Solution()
    val s1    = Solution(List(interval(1, 10)))
    val s2    = Solution(List(interval(5, 6), interval(9, 12)))

    assert(empty === empty.reduce(s1))
    assert(empty === empty.reduce(s2))
    assert(s1 === s1.reduce(empty))
    assert(s2 === s2.reduce(empty))
  }

  test("Complex Reduce") {
    val s1 = Solution(List(interval(0, 10), interval(20, 30), interval(40, 50), interval(60, 70)))
    val s2 = Solution(List(interval(5, 6), interval(15, 35), interval(45, 55)))

    assert(
      Solution(List(interval(0, 5), interval(6, 10), interval(40, 45), interval(60, 70))) ===
        s1.reduce(s2)
    )
    assert(Solution(List(interval(15, 20), interval(30, 35), interval(50, 55))) === s2.reduce(s1))
  }

  test("Complex Reduce 2") {
    val s1 = Solution(List(interval(0, 10), interval(20, 30), interval(40, 50), interval(60, 70)))
    val s2 = Solution(List(interval(5, 25), interval(55, 100)))

    assert(Solution(List(interval(0, 5), interval(25, 30), interval(40, 50))) === s1.reduce(s2))
    assert(
      Solution(List(interval(10, 20), interval(55, 60), interval(70, 100))) ===
        s2.reduce(s1)
    )
  }

}
