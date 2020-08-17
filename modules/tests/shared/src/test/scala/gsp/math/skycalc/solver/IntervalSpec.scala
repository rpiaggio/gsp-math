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
import java.time.Instant
import org.scalacheck.Gen
import org.scalacheck.Arbitrary._

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

// Test:  diff x 2, toFullDays
  test("Contains Instant") {
    forAll { i: Interval =>
      forAll(instantInInterval(i)) { inst: Instant =>
        assert(i.contains(inst))
      }
    }
  }

  test("Not Contains Instant") {
    forAll { i: Interval =>
      forAll(instantOutsideInterval(i)) { inst: Instant =>
        assert(!i.contains(inst))
      }
    }
  }

  test("Contains Interval") {
    forAll { i: Interval =>
      forAll(
        Gen
          .zip(instantInInterval(i, includeEnd = true), instantInInterval(i, includeEnd = true))
          .suchThat(t => catsSyntaxEq(t._1) =!= t._2)
      ) { instants =>
        assert(i.contains(Interval.fromInstants.getOption(instants).get))
      }
    }
  }

  test("Not Contains Interval") {
    forAll { i: Interval =>
      forAll(
        Gen // At least one instant not in Interval
          .zip(arbitrary[Instant], instantOutsideInterval(i).suchThat(_ > i.end))
          .suchThat(t => catsSyntaxEq(t._1) =!= t._2)
      ) { instants =>
        assert(!i.contains(Interval.fromInstants.getOption(instants).get))
      }
    }
  }

  test("Abuts") {
    forAll { i: Interval =>
      for {
        before <- Interval(Instant.MIN, i.start)
        after  <- Interval(i.end, Instant.MAX)
      } yield forAll(
        Gen.oneOf(
          instantInInterval(before).map(s => Interval.unsafe(s, i.start)),
          instantInInterval(after, includeEnd = true)
            .suchThat(_ > i.end)
            .map(e => Interval.unsafe(i.end, e))
        )
      ) { i2: Interval =>
        assert(i.abuts(i2))
      }
    }
  }

  test("Not Abuts") {
    forAll { i: Interval =>
      forAll(
        arbitrary[Interval]
          .suchThat(i2 => catsSyntaxEq(i2.end) =!= i.start)
          .suchThat(i2 => catsSyntaxEq(i2.start) =!= i.end)
      ) { i2: Interval =>
        assert(!i.abuts(i2))
      }
    }
  }

  test("Overlaps") {
    forAll { i: Interval =>
      for {
        untilEnd  <- Interval(Instant.MIN, i.end)
        fromStart <- Interval(i.start, Instant.MAX)
      } yield forAll(
        Gen
          .zip(instantInInterval(untilEnd), instantInInterval(fromStart).suchThat(_ > i.start))
          .suchThat(t => catsSyntaxEq(t._1) =!= t._2)
      ) { instants =>
        assert(i.overlaps(Interval.fromInstants.getOption(instants).get))
      }
    }
  }

  test("Not Overlaps") {
    forAll { i: Interval =>
      for {
        before <- Interval(Instant.MIN, i.start)
        after  <- Interval(i.end, Instant.MAX)
      } yield forAll(
        Gen
          .oneOf(
            Gen.zip(instantInInterval(before), instantInInterval(before)),
            Gen.zip(instantInInterval(after), instantInInterval(after))
          )
          .suchThat(t => catsSyntaxEq(t._1) =!= t._2)
      ) { instants =>
        assert(!i.overlaps(Interval.fromInstants.getOption(instants).get))
      }
    }
  }

  test("Join") {
    forAll { i: Interval =>
      for {
        untilEnd  <- Interval(Instant.MIN, i.end)
        fromStart <- Interval(i.start, Instant.MAX)
      } yield forAll(
        Gen
          .zip(instantInInterval(untilEnd), instantInInterval(fromStart))
          .suchThat(t => catsSyntaxEq(t._1) =!= t._2)
      ) { instants =>
        val other = Interval.fromInstants.getOption(instants).get
        val join  = i.join(other)
        assert(join.map(_.start) === List(i.start, other.start).min.some)
        assert(join.map(_.end) === List(i.end, other.end).max.some)
      }
    }
  }

  test("Empty Join") {
    forAll { i: Interval =>
      for {
        before <- Interval(Instant.MIN, i.start)
        after  <- Interval(i.end, Instant.MAX)
      } yield forAll(
        Gen
          .oneOf(
            Gen.zip(instantInInterval(before), instantInInterval(before)),
            Gen.zip(instantInInterval(after).suchThat(_ > i.end),
                    instantInInterval(after).suchThat(_ > i.end)
            )
          )
          .suchThat(t => catsSyntaxEq(t._1) =!= t._2)
      ) { instants =>
        assert(i.join(Interval.fromInstants.getOption(instants).get).isEmpty)
      }
    }
  }

  test("Intersection") {
    forAll { i: Interval =>
      for {
        untilEnd  <- Interval(Instant.MIN, i.end)
        fromStart <- Interval(i.start, Instant.MAX)
      } yield forAll(
        Gen
          .zip(instantInInterval(untilEnd), instantInInterval(fromStart).suchThat(_ > i.start))
          .suchThat(t => catsSyntaxEq(t._1) =!= t._2)
      ) { instants =>
        val other = Interval.fromInstants.getOption(instants).get
        val join  = i.intersection(other)
        assert(join.map(_.start) === List(i.start, other.start).max.some)
        assert(join.map(_.end) === List(i.end, other.end).min.some)
      }
    }
  }

  test("Empty Intersection") {
    forAll { i: Interval =>
      for {
        before <- Interval(Instant.MIN, i.start)
        after  <- Interval(i.end, Instant.MAX)
      } yield forAll(
        Gen
          .oneOf(
            Gen.zip(instantInInterval(before), instantInInterval(before)),
            Gen.zip(instantInInterval(after).suchThat(_ > i.end),
                    instantInInterval(after).suchThat(_ > i.end)
            )
          )
          .suchThat(t => catsSyntaxEq(t._1) =!= t._2)
      ) { instants =>
        assert(i.intersection(Interval.fromInstants.getOption(instants).get).isEmpty)
      }
    }
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
