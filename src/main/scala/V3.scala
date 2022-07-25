package vector
import cats.*
import cats.syntax.{*, given}
import linear.{*,given}
import Numeric.Implicits.{given}

case class V3[B](x: B, y: B, z: B)

given R1[V3] with R2[V3] with R3[V3] with
  extension [B](p: V3[B])
    def x: B = p.x
    def y: B = p.y
    def z: B = p.z

given Apply[V3] with Foldable[V3] with
  def map[A, B](fa: V3[A])(f: A => B): V3[B] = fa match {
    case V3(x, y, z) => V3(f(x), f(y), f(z))
  }

  def ap[A, B](ff: V3[A => B])(fa: V3[A]): V3[B] = (ff, fa) match {
    case (V3(ff1, ff2, ff3), V3(b1, b2, b3)) => V3(ff1(b1), ff2(b2), ff3(b3))
  }
  def foldLeft[A, B](fa: V3[A], b: B)(f: (B, A) => B): B = f(f(f(b, fa.x), fa.y), fa.z)
  def foldRight[A, B]
    (fa: V3[A], lb: cats.Eval[B])
    (f: (A, cats.Eval[B]) => cats.Eval[B]): cats.Eval[B] = f(fa.z, f(fa.y, f(fa.x, lb)))

given Additive[V3] with Affine[V3] with Metric[V3] with
  extension[B: Numeric](p: V3[B])
    def zero: V3[B] = V3(Numeric[B].zero, Numeric[B].zero, Numeric[B].zero)

def cross[B: Numeric](v1: V3[B], v2: V3[B]): V3[B] = (v1, v2) match
  case (V3(a, b, c), V3(d, e, f)) => V3(b*f-c*e, c*d-a*f, a*e-b*d)