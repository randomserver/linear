package se.randomserver
package linear

import cats.*
import cats.syntax.{*, given}
import se.randomserver.linear

import scala.math.Numeric.Implicits.given

case class V2[T](x: T, y: T)

object V2Instances:
  given Apply[V2] with Foldable[V2] with
    def map[A, B](fa: V2[A])(f: A => B): V2[B] = fa match {
      case V2(x, y) => V2(f(x), f(y))
    }
  
    def ap[A, B](ff: V2[A => B])(fa: V2[A]): V2[B] = (ff, fa) match {
      case (V2(ff1, ff2), V2(b1, b2)) => V2(ff1(b1), ff2(b2))
    }
  
    def foldLeft[A, B](fa: V2[A], b: B)(f: (B, A) => B): B = f(f(b, fa.x), fa.y)
  
    def foldRight[A, B](fa: V2[A], lb: cats.Eval[B])(f: (A, cats.Eval[B]) => cats.Eval[B]): cats.Eval[B] =
      f(fa.y, f(fa.x, lb))
  
  
  given R1[V2] with R2[V2] with
      def x[B](p: V2[B]): B = p.x
      def y[B](p: V2[B]): B = p.y
  
  given Additive[V2] with Metric[V2] with
    def zero[B: Numeric]: V2[B] = V2(Numeric[B].zero, Numeric[B].zero)

  given Affine[V2, V2] with
    override def subtractOffset[A: Numeric](p1: V2[A], d: V2[A]): V2[A] = p1 ^-^ d

    override def addOffset[A: Numeric](p1: V2[A], d: V2[A]): V2[A] = p1 ^+^ d

    override def diffOffset[A: Numeric](p1: V2[A], p2: V2[A]): V2[A] = p1 ^-^ p2

  def crossZ[B: Numeric](p1: V2[B], p2: V2[B]): B = (p1, p2) match
    case V2(x1, y1) -> V2(x2, y2) => (x1 * y2) - (y1 * x2)
end V2Instances
