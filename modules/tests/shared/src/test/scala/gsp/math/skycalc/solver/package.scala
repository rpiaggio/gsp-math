package gsp.math.skycalc

import java.time.Instant

package object solver {
  def interval(start: Int, end: Int): Interval =
    Interval(Instant.ofEpochMilli(start.toLong), Instant.ofEpochMilli(end.toLong))
}
