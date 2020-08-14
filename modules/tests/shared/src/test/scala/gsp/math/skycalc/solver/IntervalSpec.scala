// Copyright (c) 2016-2020 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package gsp.math.skycalc.solver

import cats.tests.CatsSuite

import cats.Eq
import cats.Show
import cats.kernel.laws.discipline.EqTests
import cats.kernel.laws.discipline.OrderTests
import monocle.law.discipline.PrismTests
import gsp.math.laws.discipline.FormatTests
import gsp.math.arb._
import io.chrisdavenport.cats.time._

final class IntervalSpec extends CatsSuite {
  import ArbInterval._
  import ArbTime._

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

  // Laws
  checkAll("Eq", EqTests[Interval].eqv)
  checkAll("Order", OrderTests[Interval].order)

  // Optics
  checkAll("fromOrderedInstants", PrismTests(Interval.fromOrderedInstants))
  checkAll("fromInstants", FormatTests(Interval.fromInstants).format)
  checkAll("fromStartDuration", PrismTests(Interval.fromStartDuration))

// Test contains x 2, abuts, overlaps, join, (empty) intersection, diff x 2, toFullDays

  test("Intersection") {
    val i1 = interval(5, 10)
    val i2 = interval(2, 6)
    val i3 = interval(6, 9)
    val i4 = interval(8, 12)

    assert(i1.intersection(i2) === interval(5, 6).some)
    assert(i1.intersection(i3) === i3.some)
    assert(i3.intersection(i1) === i3.some)
    assert(i1.intersection(i4) === interval(8, 10).some)
    assert(i2.intersection(i3) === none)
  }

  // check some common and some corner cases..

  test("Diff In The Middle") {
    assert(
      List(interval(1000, 1200), interval(1300, 2000)) ===
        (interval(1000, 2000).diff(interval(1200, 1300)))
    )
  }

  test("Diff By Schedule of 2") {
    assert(
      Schedule.unsafe(List(interval(1000, 1200), interval(1300, 1500), interval(1800, 2000))) ===
        interval(1000, 2000).diff(Schedule.unsafe(List(interval(1200, 1300), interval(1500, 1800))))
    )
  }

  test("Diff By Schedule of 3") {
    assert(
      Schedule.unsafe(List(interval(1000, 1200), interval(1300, 1500), interval(1800, 1950))) ===
        interval(1000, 2000).diff(
          Schedule.unsafe(
            List(interval(1200, 1300), interval(1500, 1800), interval(1950, 2000))
          )
        )
    )
  }

  test("Diff At Start") {
    assert(List(interval(1300, 2000)) === interval(1000, 2000).diff(interval(1000, 1300)))
  }

  test("Diff At End") {
    assert(List(interval(1000, 1700)) === interval(1000, 2000).diff(interval(1700, 2000)))
  }
}
