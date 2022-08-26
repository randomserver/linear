package se.randomserver
package geometry

import linear.Affine.Point
import linear.syntax.{*, given}

import math.Ordering.Implicits.infixOrderingOps
import se.randomserver.linear.{Additive, Affine, Finite, Floating, Ix, Metric}

case class LineSegment[P[_], A](start: Point[P, A], end: Point[P, A])

object LineSegment:
  given [P[_], A: Numeric](using Additive[P], Ix[P], Finite.Aux[P, 2], Metric[P], Affine.Aux[P, P]): IsIntersectableWith[LineSegment[P, A], Point[P, A]] with
    override type Intersection = Point[P, A]

    override def intersect(ls: LineSegment[P, A], c: Point[P, A]): Option[Intersection] = ls match
      case LineSegment(a, b) =>
        val ab: Point[P, A] = a ~-~ b
        val ac: Point[P, A] = a ~-~ c
        if crossZ(ab, ac) == 0 then
          val ab: P[A] = Point.unapply(a ~-~ b)
          val ac: P[A] = Point.unapply(a ~-~ c)
          val kac = dot((ab), (ac))
          val kab = dot((ab), (ab))
          if kac < Numeric[A].zero     then None
          else if kac > kab            then None
          else if kac == Numeric[A].zero then Some(c)
          else if kac == kab           then Some(c)
          else if Numeric[A].zero < kac && kac < kab then Some(c)
          else None
        else
          None


    override def intersects(p1: LineSegment[P, A], p2: Point[P, A]): Boolean = intersect(p1, p2) match
      case Some(_) => true
      case _ => false

  /**
   * LineSegment -> LineSegment intersection
    */
  given [P[_], A: Numeric: Floating] (using Additive[P], Ix[P], Finite.Aux[P, 2], Metric[P], Affine.Aux[P,P]): IsIntersectableWith[LineSegment[P,A], LineSegment[P, A]] with
    override type Intersection = Point[P,A] | LineSegment[P, A]

    protected def inrange[A: Numeric](a: (A,A), b: (A, A)): Boolean = (a, b) match
      case (t0, t1) -> (b1, b2) =>
        (t0 <= b2) && (t1 >= b1)

    override def intersect(a: LineSegment[P, A], b: LineSegment[P, A]): Option[Intersection] = (a, b) match
      case LineSegment(p1, p2) -> LineSegment(q1, q2) =>
        val zero = Numeric[A].zero
        val one  = Numeric[A].one
        val p = Point.unapply(p1)
        val q = Point.unapply(q1)
        val r = Point.unapply(p2 ~-~ p1)  // Express the line segment as p -> p + r
        val s = Point.unapply(q2 ~-~ q1)  // Express the line segment as q -> q + s

        val rxs = crossZ(r, s)
        val qp = q ^-^ p

        if rxs == zero && (crossZ(qp, r) == zero) then // Collinear
          val t0 = dot(qp, r) / quadrance(r)
          val t1 = dot(q ^+^ s ^-^ p, r) / quadrance(r)
          val range = if dot(s,r) < zero then (t1, t0) else (t0, t1)

          val ps = List(p, p ^+^ r)
          val qs =  List(q, q ^+^ s)
          val minp = ps.minBy(quadrance(_))
          val minq = qs.minBy(quadrance(_))
          val maxp = ps.maxBy(quadrance(_))
          val maxq = qs.maxBy(quadrance(_))
          val a = if quadrance(minp) > quadrance(minq) then minp else minq
          val b = if quadrance(maxp) > quadrance(maxq) then  maxq else maxp

          if inrange(range, (zero, one)) then Some(LineSegment(Point(a), Point(b)))
                                         else None
        else if rxs == zero && crossZ(qp, r) != zero then None // Parallel and non intersecting
        else if rxs != zero then
          val t: A = crossZ(qp, s) / rxs
          val u: A = crossZ(qp, r) /  rxs
          if t >= zero && t <= one && u >= zero && u <= one then Some(Point(p ^+^ (r ^* t)))
                                                            else None
        else None

    override def intersects(p1: LineSegment[P, A], p2: LineSegment[P, A]): Boolean = intersect(p1, p2) match
      case Some(_) => true
      case None    => false
end LineSegment