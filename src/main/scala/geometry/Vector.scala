package se.randomserver
package geometry

import linear.Metric
import linear.Vector.V
import math.Numeric.Implicits.infixNumericOps

object Vector {
  def scalarMultipleOf[P[_]: Metric, A: Numeric](u: P[A], v: P[A]): Boolean =
    val m = summon[Metric[P]]
    val d = m.dot(v, u)
    val num = m.quadrance(u) * m.quadrance(v)
    num == 0 || num == d * d
}
