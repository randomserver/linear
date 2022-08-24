package se.randomserver
package geometry

import linear.{Additive, Affine, Finite, Floating, Ix, Metric, R2}
import linear.syntax.{*, given}
import geometry.Vector.*

import math.Numeric.Implicits.infixNumericOps
import se.randomserver.linear


case class Line[P[_], A](anchor: Point[P, A], dir: P[A])

object Line:
  def isParallelTo[P[_]: Metric, A: Numeric]
    (l1: Line[P, A], l2: Line[P, A]): Boolean = (l1, l2) match
      case Line(_, u) -> Line(_, v) => scalarMultipleOf(u, v)

  def onLine[P[_]: Metric, A: Numeric](p: Point[P, A], l: Line[P, A]) (using Affine.Aux[P, P]) =
    val u: P[A] = Point.unapply(p ~-~ l.anchor)
    val v: P[A] = l.dir
    p == l.anchor || scalarMultipleOf(u, v)

  given [P[_]: Metric, A: Numeric](using Affine.Aux[P, P]): IsIntersectableWith[Line[P, A], Point[P, A]] with
    override type Intersection = Point[P, A]
    override def intersect(a: Line[P, A], b: Point[P, A]) = if intersects(a, b) then Some(b) else None
    override def intersects(p1: Line[P, A], p2: Point[P, A]): Boolean = onLine(p2, p1)

  given [P[_]: Metric: Ix, A: Numeric](using Floating[A], Additive[P], Affine.Aux[P, P], Finite.Aux[P, 2]): IsIntersectableWith[Line[P, A], Line[P, A]] with
    import Floating.{*, given}
    override type Intersection = Point[P, A] | Line[P, A]

    override def intersect(a: Line[P, A], b: Line[P, A]): Option[Intersection] = (a, b) match
      case (l, l2 @ Line(q, _)) if isParallelTo(l, l2) =>  if onLine(q, l) then Some(l)
                                                                           else None
      case (Line(p, u), Line(q, v)) =>
        val (px, py) = (p ! 0, p ! 1)
        val (qx, qy) = (q ! 0, q ! 1)
        val (ux, uy) = (u ! 0, u ! 1)
        val (vx, vy) = (v ! 0, v ! 1)
        val denom    = vy * ux - vx * uy
        val alpha = (ux * (py - qy) + uy * (qx - px)) / denom

        Some(Point(Point.unapply(q) ~+^ (v ^* alpha)))

    override def intersects(p1: Line[P, A], p2: Line[P, A]): Boolean = intersect(p1, p2) match
      case Some(_: Point[P, A]) => true
      case Some(_: Line[P, A]) => true
      case _ => false


end Line