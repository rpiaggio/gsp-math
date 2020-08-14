// Copyright (c) 2016-2020 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package gsp.math.skycalc.solver

import cats.tests.CatsSuite

import cats.Eq
import cats.Show
import cats.kernel.laws.discipline.MonoidTests
import gsp.math.arb._
import cats.kernel.laws.discipline.EqTests
import gsp.math.laws.discipline.SplitEpiTests
import monocle.law.discipline.PrismTests

final class ScheduleSpec extends CatsSuite {
  import ArbSchedule._
  import ArbInterval._

  test("Equality must be natural") {
    forAll { (a: Schedule, b: Schedule) =>
      a.equals(b) shouldEqual Eq[Schedule].eqv(a, b)
    }
  }

  test("Show must be natural") {
    forAll { (a: Schedule) =>
      a.toString shouldEqual Show[Schedule].show(a)
    }
  }

  // Laws
  checkAll("Eq", EqTests[Schedule].eqv)
  checkAll("Monoid", MonoidTests[Schedule].monoid)
  // Define Order or PartialOrder
  // Monoid for intersection?

  // Optics
  checkAll("fromDisjointSortedIntervals", PrismTests(Schedule.fromDisjointSortedIntervals))
  checkAll("fromIntervals", SplitEpiTests(Schedule.fromIntervals).splitEpi)

  test("Union") {
    val s1Opt  = Schedule(List(interval(1, 2), interval(5, 9)))
    val s2Opt  = Schedule(List(interval(2, 3), interval(4, 8), interval(11, 14)))
    val result = Schedule(List(interval(1, 3), interval(4, 9), interval(11, 14)))

    assert(result === s1Opt.flatMap(s1 => s2Opt.map(s2 => s1.union(s2))))
    assert(result === s1Opt.flatMap(s1 => s2Opt.map(s2 => s2.union(s1))))
  }

  test("Intersection") {
    val s1Opt  = Schedule(List(interval(1, 2), interval(5, 9)))
    val s2Opt  = Schedule(List(interval(2, 3), interval(4, 8), interval(11, 14)))
    val result = Schedule.single(interval(5, 8)).some

    assert(result === s1Opt.flatMap(s1 => s2Opt.map(s2 => s1.intersection(s2))))
    assert(result === s1Opt.flatMap(s1 => s2Opt.map(s2 => s2.intersection(s1))))
  }

  test("Simple Diff") {
    val s = Schedule.single(interval(10, 20))

    val s1 = Schedule.Never
    val s2 = Schedule.single(interval(5, 6))
    val s3 = Schedule.single(interval(5, 15))
    val s4 = Schedule.single(interval(12, 18))
    val s5 = Schedule.single(interval(15, 25))
    val s6 = Schedule.single(interval(25, 30))
    val s7 = Schedule.single(interval(5, 25))

    assert(s === s.diff(s1))
    assert(s === s.diff(s2))
    assert(Schedule.single(interval(15, 20)) === s.diff(s3))
    assert(Schedule(List(interval(10, 12), interval(18, 20))) === s.diff(s4).some)
    assert(Schedule.single(interval(10, 15)) === s.diff(s5))
    assert(s === s.diff(s6))
    assert(s1 === s.diff(s7))
  }

  test("Empty Diff") {
    val empty = Schedule.Never
    val s1    = Schedule.single(interval(1, 10))
    val s2Opt = Schedule(List(interval(5, 6), interval(9, 12)))

    assert(empty === empty.diff(s1))
    assert(empty.some === s2Opt.map(s2 => empty.diff(s2)))
    assert(s1 === s1.diff(empty))
    assert(s2Opt === s2Opt.map(_.diff(empty)))
  }

  test("Complex Diff") {
    val s1Opt =
      Schedule(List(interval(0, 10), interval(20, 30), interval(40, 50), interval(60, 70)))
    val s2Opt = Schedule(List(interval(5, 6), interval(15, 35), interval(45, 55)))

    assert(
      Schedule(List(interval(0, 5), interval(6, 10), interval(40, 45), interval(60, 70))) ===
        s1Opt.flatMap(s1 => s2Opt.map(s2 => s1.diff(s2)))
    )
    assert(
      Schedule(List(interval(15, 20), interval(30, 35), interval(50, 55))) === s1Opt.flatMap(s1 =>
        s2Opt.map(s2 => s2.diff(s1))
      )
    )
  }

  test("Complex Diff 2") {
    val s1Opt =
      Schedule(List(interval(0, 10), interval(20, 30), interval(40, 50), interval(60, 70)))
    val s2Opt = Schedule(List(interval(5, 25), interval(55, 100)))

    assert(
      Schedule(List(interval(0, 5), interval(25, 30), interval(40, 50))) === s1Opt.flatMap(s1 =>
        s2Opt.map(s2 => s1.diff(s2))
      )
    )
    assert(
      Schedule(List(interval(10, 20), interval(55, 60), interval(70, 100))) ===
        s1Opt.flatMap(s1 => s2Opt.map(s2 => s2.diff(s1)))
    )
  }

}
