// Copyright (c) 2016-2020 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package gsp.math.skycalc.solver

import cats.implicits._
import java.time.Duration
import java.time.Instant
import io.chrisdavenport.cats.time._
import scala.annotation.tailrec

/**
  * Representation of an algorithm that finds all intervals between a start and end point in time for which a given
  * function <code>f(t: Long): Boolean</code> is true.
  */
trait Solver[A] {
  def solve[R, G](constraint: Constraint[R, A], interval: Interval, calc: Calculator[R, G])(implicit
    getter:                   ResultValueGetter[G, A]
  ): Schedule
}

/**
  * Solver that finds all intervals for which the underlying constraint is true by sampling the constraint function
  * at a given rate. Intervals shorter than the sampling rate may be missed by this solver. Use this solver
  * for complex functions which can have an arbitrary amount of separate intervals for which the constraint
  * meats its criteria (e.g. Sky Background).
  */
case class DefaultSolver[A](step: Duration = Duration.ofSeconds(30)) extends Solver[A] {
  require(step > Duration.ZERO)

  def solve[R, G](constraint: Constraint[R, A], interval: Interval, calc: Calculator[R, G])(implicit
    getter:                   ResultValueGetter[G, A]
  ): Schedule = {

    @tailrec
    def solve(curStart: Instant, curState: Boolean, i: Instant, accum: Schedule): Schedule =
      if (i >= interval.end)
        if (curState)
          accum.extend(Interval.unsafe(curStart, interval.end)).get
        else
          accum
      else if (constraint.metAt(i, calc)(getter) == curState)
        solve(curStart, curState, i.plus(step), accum)
      else if (curState)
        solve(i, curState = false, i.plus(step), accum.extend(Interval.unsafe(curStart, i)).get)
      else
        solve(i, curState = true, i.plus(step), accum)

    solve(interval.start,
          constraint.metAt(interval.start, calc)(getter),
          interval.start,
          Schedule.Never
    )
  }
}

/**
  * Finds a solution for a constraint on a parabolic curve that crosses that constraint
  * at most twice during the given interval. This is true for all basic elevation constraints for a single night.
  */
case class ParabolaSolver[A](tolerance: Duration = Duration.ofSeconds(30)) extends Solver[A] {
  require(tolerance > Duration.ZERO)

  def solve[R, G](constraint: Constraint[R, A], interval: Interval, calc: Calculator[R, G])(implicit
    getter:                   ResultValueGetter[G, A]
  ): Schedule = {

    def solve(s: Instant, fs: Boolean, e: Instant, fe: Boolean): Schedule = {
      val m  = Instant.ofEpochMilli((s.toEpochMilli + e.toEpochMilli) / 2)
      val fm = constraint.metAt(m, calc)
      if (Interval.unsafe(s, e).duration > tolerance)
        (fs, fm, fe) match {
          case (false, false, false) => solve(s, fs, m, fm).extend(solve(m, fm, e, fe)).get
          case (false, false, true)  => solve(m, fm, e, fe)
          case (false, true, false)  => solve(s, fs, m, fm).extend(solve(m, fm, e, fe)).get
          case (false, true, true)   => solve(s, fs, m, fm).extend(Schedule.unsafe(m, e)).get
          case (true, false, false)  => solve(s, fs, m, fm)
          case (true, false, true)   => solve(s, fs, m, fm).extend(solve(m, fm, e, fe)).get
          case (true, true, false)   => Schedule.unsafe(s, m).extend(solve(m, fm, e, fe)).get
          case (true, true, true)    => solve(s, fs, m, fm).extend(solve(m, fm, e, fe)).get
        }
      else if (fm)
        Schedule.single(Interval.unsafe(s, e))
      else
        Schedule.Never

    }

    val fs = constraint.metAt(interval.start, calc)
    val fe = constraint.metAt(interval.end, calc)
    solve(interval.start, fs, interval.end, fe)
  }
}
