package se.randomserver
package linear

import cats.{Applicative, Apply, Foldable, Functor, Show}
import cats.syntax.show.{*, given}

import scala.collection.immutable as IM
import scala.math.Numeric.Implicits.infixNumericOps
import scala.reflect.ClassTag



trait Finite[P[_]]:
  import Vector.V
  type Size <: Int
  def toV[A](p: P[A]): V[Size, A]
  def fromV[A](v: V[Size ,A]): P[A]

import scala.compiletime.*

trait Ix[P[_]]:
  inline def checkArity(n: Int)(using f: Finite[P]): Int = if n >= constValue[f.Size] then error("Wrong arity") else n
  def elem[B](p: P[B], n: Int)(using f: Finite[P]): B
  extension [B](p: P[B])(using f: Finite[P])
    inline def !(n: Int): B =  elem[B](p, checkArity(n))

object Finite:
  type Aux[P[_], N <: Int] = Finite[P] {
    type Size = N
  }

trait Dim[P[_]]:
  def dim[A](a: P[A]): Int

object Vector {
  case class V[N <: Int, A](val elems: IM.Vector[A])

  object V:
    def apply[N <: Int, A](elems: A*): V[N, A] = V(IM.Vector(elems: _*))
    def unapply[N <: Int, A](v: V[N, A]): IM.Vector[A] = v.elems

  given [N <: Int, A: Show](using nt: ValueOf[N], at: ClassTag[A]): Show[V[N, A]] with
    def show(t: V[N, A]): String = s"V[${nt.value}, $at](${V.unapply(t).map(_.show).mkString(",")})"

  given [N <: Int]: Apply[V[N, _]] with
    override def ap[A, B](ff: V[N, A => B])(fa: V[N, A]): V[N, B] = V(
      V.unapply(ff).zip(V.unapply(fa)).map {
        case (aToB, value) => aToB.apply(value)
      }
    )

    override def map[A, B](fa: V[N, A])(f: A => B): V[N, B] = V(V.unapply(fa).map(f))

  given [N <: Int : ValueOf](using app: Apply[V[N, _]]): Applicative[V[N, _]] with
    override def pure[A](x: A): V[N, A] = V(IM.Vector.fill(valueOf[N])(x))

    override def ap[A, B](ff: V[N, A => B])(fa: V[N, A]): V[N, B] = app.ap(ff)(fa)

  given [N <: Int]: Foldable[V[N, _]] with
    def foldLeft[A, B](fa: V[N, A], b: B)(f: (B, A) => B): B = V.unapply(fa).foldLeft(b)(f)

    def foldRight[A, B]
      (fa: V[N, A], lb: cats.Eval[B])
      (f: (A, cats.Eval[B]) => cats.Eval[B]): cats.Eval[B] = summon[Foldable[IM.Vector]].foldRight(V.unapply(fa), lb)(f)

  given [N <: Int: ValueOf](using A: Applicative[V[N, _]]): Additive[V[N, _]] with
    override def zero[B: Numeric]: V[N, B] = A.pure(summon[Numeric[B]].zero)

  given [N <: Int: ValueOf]: Affine[V[N, _]] with Metric[V[N, _]] with
    override type Diff[AA] = V[N, AA]

    override def addOffset[A: Numeric](p1: V[N, A], d: Diff[A]): V[N, A] = p1 ^+^ d

    override def diffOffset[A: Numeric](p1: V[N, A], p2: V[N, A]): Diff[A] = p1 ^-^ p2

    override def subtractOffset[A: Numeric](p1: V[N, A], d: Diff[A]): V[N, A] = p1 ^-^ d

    given [P[_], A](using f: Finite[P]): Conversion[P[A], V[f.Size, A]] with
      override def apply(x: P[A]): V[f.Size, A] = f.toV(x)

  def toV[P[_], A, N <: Int](p: P[A])(using f: Finite[P]): V[f.Size, A] = f.toV(p)

  given [N <: Int]: Finite[V[N, _]] with
    override type Size = N

    override def fromV[A](v: V[Size, A]): V[N, A] = identity(v)

    override def toV[A](p: V[N, A]): V[Size, A] = identity(p)

  given [N <: Int]: Ix[V[N, _]] with
    override def elem[B](p: V[N, B], n: Int)(using f: Finite[V[N, _]]): B = p.elems(n)
    
  
  def crossZ[P[_], A: Numeric](a: P[A], b: P[A])(using Finite.Aux[P, 2], Ix[P]) =
    (a ! 0) * (b ! 1) - (a ! 1) * (b ! 0)

}
