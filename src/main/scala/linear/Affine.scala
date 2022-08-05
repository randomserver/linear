package se.randomserver
package linear

import cats.*
import cats.syntax.{*, given}
import linear.Floating.{*, given}
import linear.Additive.{*,given}
import scala.annotation.targetName
import scala.math.Fractional.Implicits.{*, given}
import scala.math.Numeric.Implicits.{*, given}

trait Affine[P[_], Diff[_]]:
  def diffOffset[A: Numeric](p1: P[A], p2: P[A]): Diff[A]
  def addOffset[A: Numeric](p1: P[A], d: Diff[A]): P[A]
  def subtractOffset[A: Numeric](p1: P[A], d: Diff[A]): P[A]

  extension [A: Numeric](p: P[A])
    def ~-~(p2: P[A]): Diff[A] = diffOffset(p, p2)
    def ~+^(o: Diff[A]): P[A] = addOffset(p, o)
    def ~-^(o: Diff[A]): P[A] = subtractOffset(p, o)

end Affine


object Affine:

  //def apply[P[_]](using a: Affine[P, _]): Affine[P, _] = a


  def qdA[Diff[_]: Apply: Foldable, P[_], B: Numeric](p1: P[B], p2: P[B])(using A: Affine[P, Diff]): B =
    summon[Foldable[Diff]].sumAll(summon[Apply[Diff]].fmap(A.diffOffset(p2, p1))(b => b * b))

  def distanceA[Diff[_]: Apply: Foldable, P[_], B: Floating](p1: P[B], p2: P[B])(using A: Affine[P, Diff]): B = sqrt(qdA(p1, p2))

  opaque type Point[P[_], A] = P[A]

  object Point:
    def apply[P[_], A](p: P[A]): Point[P, A] = p
    def unapply[P[_], A](p: Point[P, A]): P[A] = p

  given [P[_]](using A: Affine[P, P]): Affine[Point[P, _], Point[P, _]] with
    override def addOffset[A: Numeric](p1: Point[P, A], d: Point[P, A]): Point[P, A] = A.addOffset(p1, d)

    override def diffOffset[A: Numeric](p1: P[A], p2: P[A]): Point[P, A] = A.diffOffset(p1, p2)

    override def subtractOffset[A: Numeric](p1: Point[P, A], d: Point[P,A]): Point[P, A] = A.subtractOffset(p1, d)

end Affine