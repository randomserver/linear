package linear.metric

import linear.vector.{*, given}
import cats.{Apply, Foldable}
import Numeric.Implicits.{given, *}
import Fractional.Implicits.{given, *}

trait Metric[P[_]: Additive: Foldable: Apply]:
  def dot[B: Numeric](p1: P[B], p2: P[B]): B = Foldable[P].sumAll(
    Apply[P].map2(p1, p2) {
      case (a1, a2) => a1 * a2
    }
  )
  def quadrance[B: Numeric](p: P[B]): B = dot(p, p)
  def qd[B: Numeric](p1: P[B], p2: P[B]): B = quadrance(p1 - p2)
  def distance(p1: P[Double], p2: P[Double]): Double = norm(p1 - p2)
  def norm(p: P[Double]): Double = Math.sqrt(quadrance(p))
  def normalize(p: P[Double]) =
    val l = norm(p)
    Apply[P].map(p)(_ / l)

object Metric:
  def apply[P[_]](using m: Metric[P]) = m

extension [P[_]: Apply, B: Numeric](p1: P[B])
  def *(s: B): P[B]    = Apply[P].map(p1)(_ * s)

extension[P[_]: Apply, B: Fractional] (p1: P[B] )
  def /(s: B): P[B] = Apply[P].map(p1)(_ / s)

extension [M[_]: Additive: Apply, N[_]: Additive, B: Numeric](a: M[N[B]])
  def +(b: M[N[B]]) = Apply[M].map2(a, b)(_ + _)
  def -(b: M[N[B]]) = Apply[M].map2(a, b)(_ - _)