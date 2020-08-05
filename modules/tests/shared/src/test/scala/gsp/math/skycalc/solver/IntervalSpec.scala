// Copyright (c) 2016-2020 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package gsp.math.skycalc.solver

import cats.tests.CatsSuite

import cats.Eq
import cats.Show
import gsp.math.arb._

final class IntervalSpec extends CatsSuite {
  import ArbInterval._

  test("Equality must be natural") {
    forAll { (a: Interval, b: Interval) =>
      a.equals(b) shouldEqual Eq[Interval].eqv(a, b)
    }
  }

  test("Show must be natural") {
    forAll { (a: Interval) =>
      a.toString shouldEqual Show[Interval].show(a)
    }
  }

  test("Overlap") {
    val i1 = interval(5, 10)
    val i2 = interval(2, 6)
    val i3 = interval(6, 9)
    val i4 = interval(8, 12)

    assert(interval(5, 6) === i1.overlap(i2))
    assert(interval(6, 9) === i1.overlap(i3))
    assert(interval(6, 9) === i3.overlap(i1))
    assert(interval(8, 10) === i1.overlap(i4))

  }

  // check some common and some corner cases..

  test("Reduce By One") {
    assert(
      List(interval(1000, 1200), interval(1300, 2000)) ===
        Interval.reduce(interval(1000, 2000), List(interval(1200, 1300)))
    )
  }

  test("Reduce By Two") {
    assert(
      List(interval(1000, 1200), interval(1300, 1500), interval(1800, 2000)) ===
        Interval.reduce(interval(1000, 2000), List(interval(1200, 1300), interval(1500, 1800)))
    )
  }

  test("Reduce By Three") {
    assert(
      List(interval(1000, 1200), interval(1300, 1500), interval(1800, 1950)) ===
        Interval.reduce(interval(1000, 2000),
                        List(interval(1200, 1300), interval(1500, 1800), interval(1950, 2000))
        )
    )
  }

  test("Reduce At Head") {
    assert(
      List(interval(1300, 2000)) ===
        Interval.reduce(interval(1000, 2000), List(interval(1000, 1300)))
    )
  }

  test("Reduce At End") {
    assert(
      List(interval(1000, 1700)) ===
        Interval.reduce(interval(1000, 2000), List(interval(1700, 2000)))
    )
  }

  test("Reduce By Two Consecutive") {
    assert(
      List(interval(1000, 1200), interval(1300, 2000)) ===
        Interval.reduce(interval(1000, 2000), List(interval(1200, 1250), interval(1250, 1300)))
    )
  }

  test("Reduce By Three Consecutive") {
    assert(
      List(interval(1000, 1200), interval(1400, 2000)) ===
        Interval.reduce(interval(1000, 2000),
                        List(interval(1200, 1250), interval(1250, 1300), interval(1300, 1400))
        )
    )
  }

  test("Reduce By Two Consecutive At Head") {
    assert(
      List(interval(1200, 2000)) ===
        Interval.reduce(interval(1000, 2000), List(interval(1000, 1100), interval(1100, 1200)))
    )
  }

  test("Reduce By Two Consecutive At End") {
    assert(
      List(interval(1000, 1800)) ===
        Interval.reduce(interval(1000, 2000), List(interval(1800, 1900), interval(1900, 2000)))
    )
  }
}
