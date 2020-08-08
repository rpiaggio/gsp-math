// Copyright (c) 2016-2020 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package gsp.math.skycalc

import java.time.Instant

package object solver {
  def interval(start: Int, end: Int): Interval =
    Interval(Instant.ofEpochMilli(start.toLong), Instant.ofEpochMilli(end.toLong))
}
