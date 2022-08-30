package se.randomserver
package linear

import linear.LinearIntegral.{*, given}
import linear.Additive.{*, given}

import cats.{Applicative, Apply, Foldable}
import math.Numeric.Implicits.infixNumericOps
trait Metric[P[_]: Additive](using F: Foldable[P], A: Apply[P]):
  def dot[B: Numeric](p1: P[B], p2: P[B]): B = F.sumAll(
    A.map2(p1, p2) {
      case (a1, a2) => a1 * a2
    }
  )
  def quadrance[B: Numeric](p: P[B]): B = dot(p, p)
  def qd[B: Numeric](p1: P[B], p2: P[B]): B = quadrance(p1 ^-^ p2)
  def distance[B: LinearIntegral](p1: P[B], p2: P[B]): B = norm(p1 ^-^ p2)
  def norm[B: LinearIntegral](p: P[B]): B = sqrt(quadrance(p))
  def normalize[B: LinearIntegral](p: P[B]) =
    val l = norm(p)
    A.map(p)(_ / l)

object Metric:
  def apply[P[_]](using m: Metric[P]): Metric[P] = m

  def dot[P[_]: Metric, A: Numeric](a: P[A], b: P[A]): A = Metric[P].dot(a, b)
  def quadrance[P[_]: Metric, A: Numeric](a: P[A]): A = Metric[P].quadrance(a)
end Metric