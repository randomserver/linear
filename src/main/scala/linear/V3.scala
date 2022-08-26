package se.randomserver
package linear

import linear.{R1, R2, R3}

import cats.*
import cats.syntax.{*, given}
import se.randomserver.linear.Vector.V

import scala.math.Numeric.Implicits.given

case class V3[B](x: B, y: B, z: B)

object V3Instances:
  given [A: Show]: Show[V3[A]] with
    override def show(t: V3[A]): String = s"V3(${t.x}, ${t.y}, ${t.z})"
    
  given Finite[V3] with
    type Size = 3
    override def fromV[A](v: V[Size, A]): V3[A] = V3(v ! 0, v ! 1, v ! 2)

    override def toV[A](p: V3[A]): V[Size, A] = V(p.x, p.y, p.z)

  given Ix[V3] with
    override def elem[B](p: V3[B], n: Int)(using f: Finite[V3]): B = (n, p) match
      case 0 -> V3(x, _, _) => x
      case 1 -> V3(_, y, _) => y
      case 2 -> V3(_, _, z) => z
    

  given R1[V3] with R2[V3] with R3[V3] with
      def x[B](p: V3[B]): B = p.x
      def y[B](p: V3[B]): B = p.y
      def z[B](p: V3[B]): B = p.z

  given Applicative[V3] with Apply[V3] with Foldable[V3] with
    override def pure[A](x: A): V3[A] = V3(x, x, x)
    override def map[A, B](fa: V3[A])(f: A => B): V3[B] = fa match {
      case V3(x, y, z) => V3(f(x), f(y), f(z))
    }

    override def ap[A, B](ff: V3[A => B])(fa: V3[A]): V3[B] = (ff, fa) match {
      case (V3(ff1, ff2, ff3), V3(b1, b2, b3)) => V3(ff1(b1), ff2(b2), ff3(b3))
    }
    override def foldLeft[A, B](fa: V3[A], b: B)(f: (B, A) => B): B = f(f(f(b, fa.x), fa.y), fa.z)
    override def foldRight[A, B]
      (fa: V3[A], lb: cats.Eval[B])
      (f: (A, cats.Eval[B]) => cats.Eval[B]): cats.Eval[B] = f(fa.z, f(fa.y, f(fa.x, lb)))

  given Additive[V3] with Metric[V3] with
    def zero[B: Numeric]: V3[B] = V3(Numeric[B].zero, Numeric[B].zero, Numeric[B].zero)

  def cross[B: Numeric](v1: V3[B], v2: V3[B]): V3[B] = (v1, v2) match
    case (V3(a, b, c), V3(d, e, f)) => V3(b*f-c*e, c*d-a*f, a*e-b*d)

end V3Instances