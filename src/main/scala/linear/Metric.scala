package se.randomserver
package linear

import linear.Floating.{*, given}
import linear.Additive.{*, given}
import cats.{Apply, Foldable}
import scala.math.Fractional.Implicits.{*, given}
import scala.math.Numeric.Implicits.{*, given}

trait Metric[P[_]: Additive: Foldable: Apply]:
  def dot[B: Numeric](p1: P[B], p2: P[B]): B = Foldable[P].sumAll(
    Apply[P].map2(p1, p2) {
      case (a1, a2) => a1 * a2
    }
  )
  def quadrance[B: Numeric](p: P[B]): B = dot(p, p)
  def qd[B: Numeric](p1: P[B], p2: P[B]): B = quadrance(p1 ^-^ p2)
  def distance[B: Floating](p1: P[B], p2: P[B]): B = norm(p1 ^-^ p2)
  def norm[B: Floating](p: P[B]): B = sqrt(quadrance(p))
  def normalize[B: Floating](p: P[B]) =
    val l = norm(p)
    Apply[P].map(p)(_ / l)

object Metric:
  def apply[P[_]](using m: Metric[P]) = m
end Metric