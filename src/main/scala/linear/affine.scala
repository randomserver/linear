package linear.affine

import cats.syntax.{*, given}
import cats.*
import linear.{R1, R2}
import linear.vector.Additive

import Numeric.Implicits.{*, given}
import Fractional.Implicits.{*, given}

trait Affine[P[_]: Foldable: Additive: Apply]:
  def qdA[B: Numeric](p1: P[B], p2: P[B]): B =
    Foldable[P].sumAll(Functor[P].fmap(p2 - p1)(b => b * b))

  def distanceA(p1: P[Double], p2: P[Double]): Double = Math.sqrt(qdA(p1, p2))

object Affine:
  def apply[P[_]](using a: Affine[P]) = a

type PointType = [P[_]] =>> [A] =>> Point[P, A]
case class Point[P[_], A](unP: P[A])

given [P[_]: R1] : R1[PointType[P]] with
  extension [B](p: Point[P, B])
    def x: B = p.unP.x

given [P[_]: R2: R1]: R2[PointType[P]] with
  extension [B](p: Point[P, B])
    def y: B = p.unP.y

given [P[_]: Apply: Foldable]: Apply[PointType[P]] with Foldable[PointType[P]] with
  override def map[A, B](fa: PointType[P][A])(f: A => B): PointType[P][B] = fa match
    case Point(unP) => Point(Apply[P].map[A, B](unP)(f))

  override def ap[A, B](ff: PointType[P][A => B])(fa: PointType[P][A]): PointType[P][B] = (ff, fa) match {
    case (Point(unFF), Point(unFA)) => Point(
      Apply[P].ap(unFF)(unFA)
    )
  }

  override def foldLeft[A, B](fa: PointType[P][A], b: B)(f: (B, A) => B): B = Foldable[P].foldLeft(fa.unP, b)(f)

  override def foldRight[A, B](fa: PointType[P][A], lb: Eval[B])(f: (A, Eval[B]) => Eval[B]): Eval[B] = Foldable[P].foldRight(fa.unP, lb)(f)

given [P[_]: Affine: Additive: Foldable: Apply]: Additive[PointType[P]] with Affine[PointType[P]] with
  override def distanceA(p1: PointType[P][Double], p2: PointType[P][Double]): Double = Affine[P].distanceA(p1.unP, p2.unP)

  override def qdA[B: Numeric](p1: PointType[P][B], p2: PointType[P][B]): B = Affine[P].qdA(p1.unP, p2.unP)

  extension [B: Numeric](p: PointType[P][B])
    def zero: PointType[P][B] = Point(p.unP.zero)