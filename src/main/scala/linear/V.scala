package se.randomserver
package linear

import cats.{Applicative, Apply, Functor, Show}
import cats.syntax.functor.{*, given}
import cats.syntax.show.{*, given}

import scala.collection.immutable as IM
import scala.reflect.ClassTag



trait Finite[P[_]]:
  import Vector.V
  type Size[P[_]] <: Int

  def toV[A](p: P[A]): V[Size[P], A]
  def fromV[A](v: V[Size[P] ,A]): P[A]

trait Dim[P[_]]:
  def dim[A](a: P[A]): Int

object Vector {
  opaque type V[N, A] = IM.Vector[A]

  object V:
    def apply[N, A](elems: A*): V[N, A] = IM.Vector(elems: _*)
    def apply[N, A](unV: IM.Vector[A]): V[N, A] = unV
    def unapply[N, A](v: V[N, A]): IM.Vector[A] = v

  given [N, A: Show](using nt: ValueOf[N], at: ClassTag[A]): Show[V[N, A]] with
    def show(t: V[N, A]): String = s"V[${nt.value}, $at](${V.unapply(t).map(_.show).mkString(",")})"

  given [N <: Int, VN <: [A1] =>> V[N, A1]] (using Nv: ValueOf[N]): Applicative[VN] with
    override def pure[A](x: A): VN[A] = V(IM.Vector.iterate(x, Nv.value)(identity)).asInstanceOf[VN[A]]

    override def ap[A, B](ff: VN[A => B])(fa: VN[A]): VN[B] = V[N,B](
      ff.zip(fa).map {
        case (aToB, value) => aToB.apply(value)
      }
    ).asInstanceOf[VN[B]]

  given [N <: Int, VN <: [A] =>> V[N, A] : Applicative]: Additive[VN] with
    override def zero[B: Numeric]: VN[B] = V(
      summon[Applicative[VN]].pure(
        summon[Numeric[B]].zero
      )
    ).asInstanceOf[VN[B]]

  given [N <: Int, VN <: [A] =>> V[N, A] : Additive]: Affine[VN] with
    override type Diff[AA] = VN[AA]

    override def addOffset[A: Numeric](p1: VN[A], d: Diff[A]): VN[A] = p1 ^+^ d

    override def diffOffset[A: Numeric](p1: VN[A], p2: VN[A]): Diff[A] = p1 ^-^ p2

    override def subtractOffset[A: Numeric](p1: VN[A], d: Diff[A]): VN[A] = p1 ^-^ d
}
