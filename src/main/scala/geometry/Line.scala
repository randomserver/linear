package se.randomserver
package geometry

import linear.{Affine, Finite, Floating, Metric, Additive}
import linear.syntax.{*, given}

case class Line[P[_], A](anchor: Point[P, A], dir: P[A])

object Line:

  //def scalarMultipleOf[A: Floating, N <: Int](v1: V[N, A], v2: V[N, A]) = v1 ^+^ v2

  def onLine[P[_], A: Floating](p: Point[P, A], l: Line[P, A])(using a: Affine.Aux[P, P], f: Finite[P], fp: Finite[[AA] =>> Point[P, AA]]) =
    p == l.anchor //|| scalarMultipleOf(fp.toV(p ~-~ l.anchor), f.toV(l.dir))
  given [PA[_]: Metric: Affine, A: Floating](using a: Affine.Aux[PA, PA], f: Finite[PA], fp: Finite[[AA] =>> Point[PA, AA]]): Intersects[Line[PA, A], Point[PA, A]] with
    override def intersects(p1: Line[PA, A], p2: Point[PA, A]): Boolean = ??? //onLine(p2, p1)
end Line

