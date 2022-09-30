package se.randomserver
package linear

import cats.*
import cats.syntax.{*, given}
import se.randomserver.linear

import scala.math.Numeric.Implicits.given
import linear.Vector.V

case class V2[T](x: T, y: T)

object V2Instances:

  given[A: Show]: Show[V2[A]] with
    override def show(t: V2[A]): String = s"V2(${t.x}, ${t.y})"

  given Arity[V2] with
    type Size = 2

    override def toV[A](p: V2[A]): V[Size, A] = V(p.x, p.y)

    override def fromV[A](v: V[Size, A]): V2[A] = V2(v ! 0, v ! 1)

  given Ix[V2] with
    override def elem[B](p: V2[B], n: Int)(using Arity[V2]): B = (n, p)  match
      case 0 -> V2(x, _) => x
      case 1 -> V2(_, y) => y

  given Applicative[V2] with Apply[V2] with Foldable[V2] with
    override def pure[A](x: A): V2[A] = V2(x, x)

    override def map[A, B](fa: V2[A])(f: A => B): V2[B] = fa match {
      case V2(x, y) => V2(f(x), f(y))
    }
  
    override def ap[A, B](ff: V2[A => B])(fa: V2[A]): V2[B] = (ff, fa) match {
      case (V2(ff1, ff2), V2(b1, b2)) => V2(ff1(b1), ff2(b2))
    }
  
    override def foldLeft[A, B](fa: V2[A], b: B)(f: (B, A) => B): B = f(f(b, fa.x), fa.y)
  
    override def foldRight[A, B](fa: V2[A], lb: cats.Eval[B])(f: (A, cats.Eval[B]) => cats.Eval[B]): cats.Eval[B] =
      f(fa.y, f(fa.x, lb))
  
  
  given R1[V2] with R2[V2] with
      def x[B](p: V2[B]): B = p.x
      def y[B](p: V2[B]): B = p.y

  
  given Additive[V2] with Metric[V2] with
    def zero[B: Numeric]: V2[B] = V2(Numeric[B].zero, Numeric[B].zero)

end V2Instances
