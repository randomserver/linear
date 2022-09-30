package se.randomserver
package linear

import cats.{Applicative, Apply, Foldable, Functor, Show}
import cats.syntax.show.{*, given}
import se.randomserver.linear.Vector.V

import scala.collection.immutable as IM
import scala.math.Numeric.Implicits.infixNumericOps
import scala.reflect.ClassTag
import scala.compiletime.*

trait Ix[P[_]]:
  inline def checkArity(n: Int)(using f: Arity[P]): Int = if n >= constValue[f.Size] then error(s"Wrong arity") else n
  def elem[B](p: P[B], n: Int)(using f: Arity[P]): B
  extension [B](p: P[B])(using f: Arity[P])
    inline def !(n: Int): B =  elem[B](p, checkArity(n))

object Arity:
 type Aux[P[_], N <: Int] = Arity[P] {
  type Size = N
 }
end Arity

trait Arity[P[_]]:
  type Size <: Int

  def toV[A](p: P[A]): Vector.V[Size, A]
  def fromV[A](v: Vector.V[Size, A]): P[A]

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


  given [N <: Int]: Arity[V[N, _]] with
    override type Size = N

    override def toV[A](p: V[Size, A]): V[Size, A] = identity(p)
    override def fromV[A](v: V[Size, A]): V[Size, A] = identity(v)


  given [N <: Int]: Ix[V[N, _]] with
    override def elem[B](p: V[N, B], n: Int)(using f: Arity[V[N, _]]): B = p.elems(n)
    
  
  def crossZ[P[_], A: Numeric](a: P[A], b: P[A])(using Arity.Aux[P, 2], Ix[P]) =
    (a ! 0) * (b ! 1) - (a ! 1) * (b ! 0)

  def lerp[P[_]: Additive, A: Numeric](alpha: A, u: P[A], v: P[A]): P[A] = (alpha *^ u) ^+^ ((Numeric[A].one - alpha) *^ v)
  def toV[P[_], A](p: P[A])(using a: Arity[P]): V[a.Size, A] = a.toV(p)
  def fromV[P[_], A, N <: Int](v: V[N, A])(using a: Arity.Aux[P, N]): P[A] = a.fromV(v)
}
