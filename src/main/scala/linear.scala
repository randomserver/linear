package linear

import cats._
import cats.syntax.{given, *}
import Numeric.Implicits.{given, *}
import Fractional.Implicits.{given, *}


trait R1[P[_]]:
  extension [B](p: P[B])
    def x: B

trait R2[P[_]: R1]:
  extension [B](p: P[B])
    def y: B
    
trait R3[P[_]: R2]:
  extension [B](p: P[B])
    def z: B

trait Additive[P[_]: Apply]:
  extension [B: Numeric](p: P[B])
    def +(o: P[B]): P[B] = Apply[P].map2(p, o) {
      case (aa, bb) => aa + bb
    }
    def -(o: P[B]): P[B] = p + o.neg
    def zero: P[B]
    def neg: P[B] = Apply[P].map(p)(b => -b)


trait Affine[P[_]: Foldable: Additive: Apply]:
  def qdA[B: Numeric](p1: P[B], p2: P[B]): B =
    Foldable[P].sumAll(Functor[P].fmap(p2 - p1)(b => b * b))

  def distanceA(p1: P[Double], p2: P[Double]): Double = Math.sqrt(qdA(p1, p2))

object Affine:
  def apply[P[_]](using a: Affine[P]) = a

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