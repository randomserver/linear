package se.randomserver
package linear

import cats.*
import cats.syntax.{*, given}
import linear.Floating.{*, given}
import linear.Additive.{*, given}

import se.randomserver.linear.Vector.V

import scala.annotation.targetName
import scala.math.Fractional.Implicits.{*, given}
import scala.math.Numeric.Implicits.{*, given}
import scala.reflect.ClassTag

trait Affine[P[_]]:
  type Diff[_]
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

  type Aux[P[_], D[_]] = Affine[P] {
    type Diff[AA] = D[AA]
  }

  def qdA[P[_], B: Numeric](p1: P[B], p2: P[B])(using A: Affine[P], F: Foldable[A.Diff], App: Apply[A.Diff]): B =
    F.sumAll(App.fmap(A.diffOffset(p2, p1))(b => b * b))

  def distanceA[P[_], B: Floating](p1: P[B], p2: P[B])(using A: Affine[P], F: Foldable[A.Diff], App: Apply[A.Diff]): B = sqrt(qdA(p1, p2))

  opaque type Point[P[_], A] = P[A]

  object Point:
    def apply[P[_], A](p: P[A]): Point[P, A] = p
    def unapply[P[_], A](p: Point[P, A]): P[A] = p

  given [P[_]: Foldable: Apply, PointP <: [A] =>> Point[P, A]]: Foldable[PointP] with Apply[PointP] with
    def ap[A, B](ff: PointP[A => B])(fa: PointP[A]): PointP[B] = summon[Apply[P]].ap(ff)(fa).asInstanceOf[PointP[B]]
    override def map[A, B](fa: PointP[A])(f: A => B): PointP[B] = summon[Functor[P]].map(fa)(f).asInstanceOf[PointP[B]]

    override def foldLeft[A, B](fa: PointP[A], b: B)(f: (B, A) => B): B = summon[Foldable[P]].foldLeft(fa, b)(f)
    override def foldRight[A, B]
      (fa: PointP[A], lb: cats.Eval[B])
      (f: (A, cats.Eval[B]) => cats.Eval[B]): cats.Eval[B] = summon[Foldable[P]].foldRight(fa, lb)(f)

  given [P[_]](using A: Additive[P]): Affine[P] with
    override type Diff[AA] = P[AA]

    override def subtractOffset[A: Numeric](p1: P[A], d: Diff[A]): P[A] = p1 ^-^ d

    override def addOffset[A: Numeric](p1: P[A], d: Diff[A]): P[A] = p1 ^+^ d

    override def diffOffset[A: Numeric](p1: P[A], p2: P[A]): Diff[A] = p1 ^-^ p2

  given [P[_]: Apply](using A: Additive[P], M: Metric[P]): Additive[Point[P, _]] with
    override def zero[B: Numeric]: Point[P, B] = A.zero

  given [P[_]: Apply: Foldable: Additive]: Metric[Point[P, _]] = new Metric[Point[P, _]] {}

  given [P[_], A: Show](using S: Show[P[A]]): Show[Point[P, A]] with
    override def show(t: Point[P, A]): String = s"Point(${S.show(t)})"

  given [P[_], N <: Int](using arity: Arity.Aux[P, N]): Arity[Point[P, _]] with
    type Size = arity.Size

    override def toV[A](p: Point[P, A]): V[Size, A] = arity.toV(p)

    override def fromV[A](v: V[Size, A]): Point[P, A] = arity.fromV(v)

  given [P[_]: Arity](using ix: Ix[P]): Ix[Point[P, _]] with
    override def elem[B](p: Point[P, B], n: Int)(using f: Arity[Point[P, _]]): B = ix.elem(p, n)

end Affine