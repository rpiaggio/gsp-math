// Copyright (c) 2016-2020 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.math.arb

import cats.Eval
import org.scalacheck.Arbitrary
import org.scalacheck.Arbitrary._
import org.scalacheck.Gen
import org.scalacheck.Cogen

trait ArbEval {
  def genSampleValue[A: Arbitrary]: Gen[Eval[A]] =
    arbitrary[A].map(Eval.now)

  implicit def arbSampleValue[A: Arbitrary]: Arbitrary[Eval[A]] =
    Arbitrary(genSampleValue[A])

  implicit def cogenEval[A: Cogen]: Cogen[Eval[A]] =
    Cogen[A].contramap(_.value)
}

object ArbEval extends ArbEval
