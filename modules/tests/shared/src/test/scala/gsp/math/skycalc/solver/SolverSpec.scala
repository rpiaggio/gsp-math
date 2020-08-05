package gsp.math.skycalc.solver

import cats.tests.CatsSuite
import scala.annotation.unused
import java.time.Instant
import java.time.Duration

class SolverSpec extends CatsSuite {

  case class TestConstraint(solver: Solver[Long], f: Instant => Boolean) extends Constraint[Long] {
    def metAt(i: Instant, @unused dummy: Long) = f(i)
  }

  def f1(i: Instant) =
    if (i.toEpochMilli < 150) true
    else if (i.toEpochMilli >= 250 && i.toEpochMilli < 450) true
    else false

  test("Check Default Solver") {
    val c = new TestConstraint(DefaultSolver(Duration.ofMillis(1)), f1)
    assert(Solution(List(interval(0, 150))) === c.solve(interval(0, 200), 0))
    assert(Solution(List(interval(250, 400))) === c.solve(interval(200, 400), 0))
    assert(Solution(List(interval(250, 450))) === c.solve(interval(200, 500), 0))
    assert(
      Solution(List(interval(0, 150), interval(250, 400))) ===
        c.solve(interval(0, 400), 0)
    )
  }

  test("Check Parabola Solver") {
    val c = new TestConstraint(ParabolaSolver(Duration.ofMillis(1)), f1)
    assert(Solution(List(interval(0, 150))) === c.solve(interval(0, 200), 0))
    assert(Solution(List(interval(250, 400))) === c.solve(interval(200, 400), 0))
    assert(Solution(List(interval(250, 450))) === c.solve(interval(200, 500), 0))
    assert(
      Solution(List(interval(0, 150), interval(250, 400))) ===
        c.solve(interval(0, 400), 0)
    )
  }

  def f2(i: Instant) =
    if (i.toEpochMilli >= 5000 && i.toEpochMilli < 6000) true
    else false

  test("Check Parabola Solver 2") {
    val c = new TestConstraint(ParabolaSolver(Duration.ofMillis(1)), f2)
    assert(
      (Solution(List(interval(5000, 6000))) === c.solve(interval(0, 10000), 0))
    )
  }
}
