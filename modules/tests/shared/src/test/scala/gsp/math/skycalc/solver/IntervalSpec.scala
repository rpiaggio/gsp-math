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
import java.time.ZoneId
import java.time.LocalTime
import java.time.Duration

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
      forAll(instantOutsideInterval(i)) { instOpt =>
        assert(instOpt.forall(inst => !i.contains(inst)))
      }
    }
  }

  test("Contains Interval") {
    forAll { i: Interval =>
      forAll(
        distinctZip(instantInInterval(i, includeEnd = true),
                    instantInInterval(i, includeEnd = true)
        )
      ) { instants =>
        assert(Interval.fromInstants.getOption(instants).exists(i.contains))
      }
    }
  }

  test("Not Contains Interval") {
    forAll { i: Interval =>
      forAll(
        // At least one instant not in Interval
        distinctZip(Gen.some(instantWithSpecialInterval(i)),
                    instantOutsideInterval(i, includeEnd = false)
        )
      ) { instants =>
        assert(
          instants
            .mapN(Function.untupled(Interval.fromInstants.getOption))
            .flatten
            .forall(other => !i.contains(other))
        )
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
      forAll(
        distinctZip(untilEndOfInterval(i), fromStartOfInterval(i, includeStart = false))
      ) { instants =>
        assert(Interval.fromInstants.getOption(instants).exists(i.overlaps))
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
            distinctZip(instantInInterval(before), instantInInterval(before)),
            distinctZip(instantInInterval(after), instantInInterval(after))
          )
      ) { instants =>
        assert(Interval.fromInstants.getOption(instants).exists(other => !i.overlaps(other)))
      }
    }
  }

  test("Join") {
    forAll { i: Interval =>
      forAll(
        distinctZip(untilEndOfInterval(i, includeEnd = true), fromStartOfInterval(i))
      ) { instants =>
        Interval.fromInstants.getOption(instants).foreach { other =>
          val join = i.join(other)
          assert(join.map(_.start) === List(i.start, other.start).min.some)
          assert(join.map(_.end) === List(i.end, other.end).max.some)
        }
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
            distinctZip(instantInInterval(before), instantInInterval(before)),
            distinctZip(instantInInterval(after, includeStart = false),
                        instantInInterval(after, includeStart = false)
            )
          )
      ) { instants =>
        assert(Interval.fromInstants.getOption(instants).exists(other => i.join(other).isEmpty))
      }
    }
  }

  test("Intersection") {
    forAll { i: Interval =>
      forAll(
        distinctZip(untilEndOfInterval(i), fromStartOfInterval(i, includeStart = false))
      ) { instants =>
        Interval.fromInstants.getOption(instants).foreach { other =>
          val intersection = i.intersection(other)
          assert(intersection.map(_.start) === List(i.start, other.start).max.some)
          assert(intersection.map(_.end) === List(i.end, other.end).min.some)
        }
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
            distinctZip(instantInInterval(before), instantInInterval(before)),
            distinctZip(instantInInterval(after, includeStart = false),
                        instantInInterval(after, includeStart = false)
            )
          )
      ) { instants =>
        assert(
          Interval.fromInstants.getOption(instants).exists(other => i.intersection(other).isEmpty)
        )
      }
    }
  }

  // diff
  // original includes results
  // all results abut other (or some interval from schedule)
  // fold joining other results in original (interleave with schedule?)

  test("Diff Interval") {
    forAll { i: Interval =>
      forAll(
        // At least one end within Interval i
        distinctZip(instantWithSpecialInterval(i), instantInInterval(i, includeStart = false))
      ) { instants =>
        Interval.fromInstants.getOption(instants).foreach { other =>
          val diff = i.diff(other)
          assert(diff.nonEmpty)
          assert(diff.forall(i.contains))
          assert(diff.forall(other.abuts))
          assert(
            diff
              .foldLeft(other.some)((a, b) => a.flatMap(_.join(b)))
              .flatMap(_.intersection(i)) === i.some
          )
        }
      }
    }
  }

  test("Unmodified Diff Interval") {
    forAll { i: Interval =>
      for {
        before <- Interval(Instant.MIN, i.start)
        after  <- Interval(i.end, Instant.MAX)
      } yield forAll(
        Gen
          .oneOf(
            distinctZip(instantInInterval(before, includeEnd = true),
                        instantInInterval(before, includeEnd = true)
            ),
            distinctZip(instantInInterval(after), instantInInterval(after))
          )
      ) { instants =>
        Interval.fromInstants.getOption(instants).foreach { other =>
          assert(i.diff(other) === List(i))
        }
      }
    }
  }

  test("Empty Diff Interval") {
    forAll { i: Interval =>
      for {
        before <- Interval(Instant.MIN, i.start)
        after  <- Interval(i.end, Instant.MAX)
      } yield forAll(
        distinctZip(instantInInterval(before, includeEnd = true, specials = List(i.start)),
                    instantInInterval(after, includeEnd = true, specials = List(i.end))
        )
      ) { instants =>
        Interval.fromInstants.getOption(instants).foreach { other =>
          assert(i.diff(other).isEmpty)
        }
      }
    }
  }

  test("ToFullDays") {
    forAll { (i: Interval, z: ZoneId, t: LocalTime) =>
      val allDay = i.toFullDays(z, t)
      assert(allDay.contains(i))
      assert(allDay.start.atZone(z).toLocalTime === t)
      assert(allDay.end.atZone(z).toLocalTime === t)
      assert(allDay.diff(i).forall(_.duration < Duration.ofDays(1)))
    }
  }

  // check some common and some corner cases..

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
}
