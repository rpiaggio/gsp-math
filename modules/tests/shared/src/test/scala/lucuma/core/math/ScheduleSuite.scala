// Copyright (c) 2016-2021 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.math

import cats._
import cats.syntax.all._
import java.time.ZoneId
import java.time.LocalTime
import java.time.Instant
import org.scalacheck.Arbitrary._
import org.scalacheck.Gen
import org.scalacheck.Prop._
import lucuma.core.math.arb._
import lucuma.core.arb.ArbTime
import cats.kernel.laws.discipline.BoundedSemilatticeTests
import cats.kernel.laws.discipline.EqTests
import monocle.law.discipline.PrismTests
import lucuma.core.optics.laws.discipline.SplitEpiTests
import io.chrisdavenport.cats.time._

final class ScheduleSuite extends munit.DisciplineSuite with IntervalGens {
  import ArbSchedule._
  import ArbInterval._
  import ArbTime._

  // Reduce the search space, to make
  override def scalaCheckTestParameters =
    super.scalaCheckTestParameters
      .withMinSuccessfulTests(20)
      .withMaxSize(50)

  test("Equality must be natural") {
    forAll { (a: Schedule, b: Schedule) =>
      assertEquals(a.equals(b),  Eq[Schedule].eqv(a, b))
    }
  }

  test("Show must be natural") {
    forAll { (a: Schedule) =>
      assertEquals(a.toString,  Show[Schedule].show(a))
    }
  }

  // Laws
  checkAll("Eq", EqTests[Schedule].eqv)
  checkAll("BoundedSemilattice (Union)",
           BoundedSemilatticeTests[Schedule](Schedule.UnionBoundedSemilattice).boundedSemilattice
  )
  checkAll(
    "BoundedSemilattice (Intersection)",
    BoundedSemilatticeTests[Schedule](Schedule.IntersectionBoundedSemilattice).boundedSemilattice
  )

  // Optics
  checkAll("fromDisjointSortedIntervals", PrismTests(Schedule.fromDisjointSortedIntervals))
  checkAll("fromIntervals", SplitEpiTests(Schedule.fromIntervals).splitEpi)

  test("Contains") {
    forAll { s: Schedule =>
      forAll(instantInSchedule(s)) { iOpt =>
        assert(iOpt.forall(s.contains))
      }
    }
  }

  test("Not Contains") {
    forAll { s: Schedule =>
      forAll(instantOutsideSchedule(s)) { iOpt =>
        assert(iOpt.forall(i => !s.contains(i)))
      }
    }
  }

  test("Covers") {
    forAll { s: Schedule =>
      forAll(intervalInSchedule(s)) { iOpt =>
        assert(iOpt.forall(s.covers))
      }
    }
  }

  test("Not Covers") {
    forAll { s: Schedule =>
      // At least one end outside of schedule
      forAll(
        distinctZipOpt(arbitrary[Instant].map(_.some),
                       instantOutsideSchedule(s, includeEnds = false)
        )
      ) { instantsOpt =>
        assert(
          instantsOpt
            .flatMap(Interval.fromInstants.getOption)
            .forall(i => !s.covers(i))
        )
      }
    }
  }

  test("Overlaps") {
    forAll { s: Schedule =>
      forAll(
        if (s.isEmpty)
          Gen.const(none)
        else
          // Instants from two different sections
          distinctZip(sectionInSchedule(s), sectionInSchedule(s)).flatMap {
            case (i1, i2) =>
              (for {
                inst1 <- instantInInterval(i1)
                inst2 <- instantInInterval(i2)
              } yield Interval.fromInstants.getOption((inst1, inst2))).suchThat(_.isDefined)
          }
      ) { iOpt =>
        assert(iOpt.forall(s.overlaps))
      }
    }
  }

  test("Not Overlaps") {
    forAll { s: Schedule =>
      forAll(
        // Two instants from the same section, outside of schedule
        sectionInSchedule(s)
          .suchThat(i => !s.intervals.contains(i))
          .flatMap(i =>
            (for {
              inst1 <- instantInInterval(i)
              inst2 <- instantInInterval(i)
            } yield Interval.fromInstants.getOption((inst1, inst2))).suchThat(_.isDefined)
          )
      ) { iOpt =>
        assert(iOpt.exists(i => !s.overlaps(i)))
      }
    }
  }

  test("RestrictTo") {
    forAll { (s: Schedule, i: Interval) =>
      val restricted = s.restrictTo(i)
      assert(restricted.intervals.forall(i.contains))
      assert(restricted.intervals.forall(s.covers))
      assert(restricted.restrictTo(i) === restricted) // Idempotence
    }
  }

  test("ToFullDays") {
    forAll { (s: Schedule, z: ZoneId, t: LocalTime) =>
      val allDay = s.toFullDays(z, t)
      assert(allDay.union(s) === allDay)
      // We can't comparte LocalTimes directly since the LocalTime may not exist at
      // the given day if there was a DST transition.
      allDay.intervals.foreach { interval =>
        val start = interval.start.atZone(z)
        assert(start === start.`with`(t))
        val end   = interval.end.atZone(z)
        assert(end === end.`with`(t))
      }
    }
  }

  test("Union with Schedule") {
    forAll { (s1: Schedule, s2: Schedule) =>
      val union = s1.union(s2)
      assert(s1.intervals.forall(union.covers))
      assert(s2.intervals.forall(union.covers))
    }
  }

  test("Union with Interval") {
    forAll { (s: Schedule, i: Interval) =>
      val union = s.union(i)
      assert(s.intervals.forall(union.covers))
      assert(union.covers(i))
    }
  }

  test("Intersection with Schedule") {
    forAll { (s1: Schedule, s2: Schedule) =>
      val intersection = s1.intersection(s2)
      assert(intersection.intervals.forall(s1.covers))
      assert(intersection.intervals.forall(s2.covers))
    }
  }

  test("Intersection with Interval") {
    forAll { (s: Schedule, i: Interval) =>
      val intersection = s.intersection(i)
      assert(intersection.intervals.forall(s.covers))
      assert(intersection.intervals.forall(i.contains))
    }
  }

  test("Diff with Schedule") {
    forAll { (s1: Schedule, s2: Schedule) =>
      val diff = s1.diff(s2)
      assert(diff.intervals.forall(s1.covers))
      assert(diff.intersection(s2).isEmpty)
      assert(diff.union(s1.intersection(s2)) === s1)
      assert(diff.diff(s2) === diff) // Idempotence
    }
  }

  test("Diff with Interval") {
    forAll { (s: Schedule, i: Interval) =>
      val diff = s.diff(i)
      assert(diff.intervals.forall(s.covers))
      assert(diff.intersection(i).isEmpty)
      assert(diff.union(s.intersection(i)) === s)
      assert(diff.diff(i) === diff) // Idempotence
    }
  }

  test("Gaps") {
    forAll { s: Schedule =>
      val gaps = s.gaps
      assert(gaps.intervals.length === 0.max(s.intervals.length - 1))
      // Union with gaps must be equal to Schedule with one interval from earliest to latest.
      assert(
        s.union(gaps).some.filter(_.nonEmpty) ===
          (s.earliest, s.latest).mapN(Schedule.apply).flatten
      )
    }
  }

}
