package se.randomserver
package linear

import cats.*
import cats.syntax.{*, given}
import linear.Floating.{*, given}
import linear.Additive.{*,given}
import scala.annotation.targetName
import scala.math.Fractional.Implicits.{*, given}
import scala.math.Numeric.Implicits.{*, given}

trait Affine[P[_]]:
  type Diff[_]
  def diffOffset[A: Numeric](p1: P[A], p2: P[A]): Diff[A]
  def addOffset[A: Numeric](p1: P[A], d: Diff[A]): P[A]
  def subtractOffset[A: Numeric](p1: P[A], d: Diff[A]): P[A]

  extension [A: Numeric](p: P[A])
    @targetName("diffOffset")
    def ~-~(p1: P[A], p2: P[A]): Diff[A] = diffOffset(p1, p2)
    @targetName("addOffset")
    def ~+~(p1: P[A], o: Diff[A]): P[A] = addOffset(p1, o)
    @targetName("subtractOffset")
    def ~-~(p1: P[A], o: Diff[A]): P[A] = subtractOffset(p1, o)

end Affine


object Affine:

  //def apply[P[_]](using a: Affine[P, _]): Affine[P, _] = a


  //def qdA[P[_]: Foldable: Apply: Affine: Affine.Diff, B: Numeric](p1: P[B], p2: P[B]): B =
  //  summon[Foldable[P]].sumAll(summon[Apply[P]].fmap(Affine[P].diffOffset(p2, p1))(b => b * b))

  //def distanceA[P[_]: Affine: Foldable: Apply, B: Floating](p1: P[B], p2: P[B]): B = sqrt(qdA(p1, p2))

  //opaque type Point[P[_], A] = A => P[A]

  //given [P[_]: Additive]: Affine[Point[P, _]] with
  //  override def addOffset[A: Numeric](p1: Point[P, A], d: Point[P, A]): Point[P, A] = p1 ^+^ d

end Affine