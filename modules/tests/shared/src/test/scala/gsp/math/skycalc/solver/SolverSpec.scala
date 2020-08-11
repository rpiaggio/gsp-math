// Copyright (c) 2016-2020 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package gsp.math.skycalc.solver

import cats.tests.CatsSuite
import java.time.Instant
import java.time.Duration

class SolverSpec extends CatsSuite {
  class TestCalculator extends Calculator[Unit, GetterStrategy.Exact] {
    override val instants: List[Instant] = List.empty

    override def toIndex(i: Instant): Int = 0

    override val result: Instant => Unit = _ => ()
  }

  case class TestConstraint(solver: Solver[Unit], f: Instant => Boolean)
      extends Constraint[Unit, Unit] {

    override def metAt[G](i: Instant, calc: Calculator[Unit, G])(implicit
      getter:                ResultValueGetter[G, Unit]
    ): Boolean = f(i)
  }

  val testCalculator = new TestCalculator

  implicit val testValueGetter = new ResultValueGetter[GetterStrategy.Exact, Unit] {
    def get[Results](
      timedResults: List[(Instant, Results)],
      toIndex:      Instant => Int,
      field:        Results => Unit,
      instant:      Instant
    ): Unit = ()
  }

  def f1(i: Instant) =
    if (i.toEpochMilli < 150) true
    else if (i.toEpochMilli >= 250 && i.toEpochMilli < 450) true
    else false

  test("Check Default Solver") {
    val s = DefaultSolver[Unit](Duration.ofMillis(1))
    val c = TestConstraint(s, f1)

    assert(Schedule(List(interval(0, 150))) === c.solve(interval(0, 200), testCalculator).some)
    assert(Schedule(List(interval(250, 400))) === c.solve(interval(200, 400), testCalculator).some)
    assert(Schedule(List(interval(250, 450))) === c.solve(interval(200, 500), testCalculator).some)
    assert(
      Schedule(List(interval(0, 150), interval(250, 400))) ===
        c.solve(interval(0, 400), testCalculator).some
    )
  }

  test("Check Parabola Solver") {
    val c = TestConstraint(ParabolaSolver[Unit](Duration.ofMillis(1)), f1)
    assert(Schedule(List(interval(0, 150))) === c.solve(interval(0, 200), testCalculator).some)
    assert(Schedule(List(interval(250, 400))) === c.solve(interval(200, 400), testCalculator).some)
    assert(Schedule(List(interval(250, 450))) === c.solve(interval(200, 500), testCalculator).some)
    assert(
      Schedule(List(interval(0, 150), interval(250, 400))) ===
        c.solve(interval(0, 400), testCalculator).some
    )
  }

  def f2(i: Instant) =
    if (i.toEpochMilli >= 5000 && i.toEpochMilli < 6000) true
    else false

  test("Check Parabola Solver 2") {
    val c = TestConstraint(ParabolaSolver[Unit](Duration.ofMillis(1)), f2)
    assert(
      (Schedule(List(interval(5000, 6000))) === c.solve(interval(0, 10000), testCalculator).some)
    )
  }
}
