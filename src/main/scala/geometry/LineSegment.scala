package se.randomserver
package geometry

import cats.{Apply, Foldable}
import linear.Affine.Point
import linear.syntax.{*, given}
import linear.LinearIntegral.{*, given}

import se.randomserver.linear.{Additive, Affine, Arity, Ix, LinearIntegral, Metric}

case class LineSegment[P[_], A](start: Point[P, A], end: Point[P, A])

object LineSegment:
  private def one[A: LinearIntegral] = Numeric[A].one
  private def zero[A: LinearIntegral] = Numeric[A].zero
  private def two[A: LinearIntegral] = one + one
  given [P[_]: Metric: Additive: Ix, A: LinearIntegral: Numeric](using Arity.Aux[P, 2], Additive[Point[P, _]], Metric[Point[P, _]]): IsIntersectableWith[LineSegment[P, A], Point[P, A]] with
    override type Intersection = Point[P, A]

    override def intersect(ls: LineSegment[P, A], c: Point[P, A]): Option[Intersection] = ls match
      case LineSegment(a, b) =>
        val ab: Point[P, A] = a ~-~ b
        val ac: Point[P, A] = a ~-~ c
        if crossZ(ab, ac) == 0 then
          val ab = a ~-~ b
          val ac = a ~-~ c
          val kac = dot(ab, ac)
          val kab = dot(ab, ab)
          if kac < zero                    then None
          else if kac > kab                then None
          else if kac == zero              then Some(c)
          else if kac == kab               then Some(c)
          else if zero < kac && kac < kab  then Some(c)
          else None
        else
          None


    override def intersects(p1: LineSegment[P, A], p2: Point[P, A]): Boolean = intersect(p1, p2) match
      case Some(_) => true
      case _ => false

  /**
   * LineSegment -> LineSegment intersection
    */
  given [P[_]: Additive: Metric: Ix, A: LinearIntegral](using Arity.Aux[P, 2], Metric[Point[P, _]], Additive[Point[P, _]]): IsIntersectableWith[LineSegment[P,A], LineSegment[P, A]] with
    override type Intersection = Point[P,A] | LineSegment[P, A]

    protected def inrange[A: Numeric](a: (A,A), b: (A, A)): Boolean = (a, b) match
      case (t0, t1) -> (b1, b2) =>
        (t0 <= b2) && (t1 >= b1)

    override def intersect(a: LineSegment[P, A], b: LineSegment[P, A]): Option[Intersection] = (a, b) match
      case LineSegment(p, p2) -> LineSegment(q, q2) =>
        val r = p2 ~-~ p  // Express the line segment as p -> p + r
        val s = q2 ~-~ q  // Express the line segment as q -> q + s

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

          if inrange(range, (zero, one)) then Some(LineSegment(a, b))
                                         else None
        else if rxs == zero && crossZ(qp, r) != zero then None // Parallel and non intersecting
        else if rxs != zero then
          val t: A = crossZ(qp, s) / rxs
          val u: A = crossZ(qp, r) /  rxs
          if t >= zero && t <= one && u >= zero && u <= one then Some(p ^+^ (r ^* t))
                                                            else None
        else None

    override def intersects(p1: LineSegment[P, A], p2: LineSegment[P, A]): Boolean = intersect(p1, p2) match
      case Some(_) => true
      case None    => false

  def qdSegment[P[_], A: LinearIntegral]
  (p: Point[P, A], segment: LineSegment[P, A])
  (using Metric[P], Affine.Aux[P,P], Foldable[P], Apply[P], Metric[Point[P, _]], Additive[Point[P, _]]): A =
    val l2 = qdA(segment.start, segment.end)
    if l2 == zero then qdA(p, segment.start)
    else
      val t = List(zero, List(one, dot(p ^-^ segment.start, segment.end ^-^ segment.start) / l2).min).max
      val proj = segment.start ^+^ ((segment.end ^-^ segment.start) ^* t)
      qdA(p, proj)

  def closestPoint[P[_], A: LinearIntegral](p: Point[P, A], segment: LineSegment[P, A])(using Affine.Aux[P, P], Foldable[P], Apply[P], Metric[Point[P, _]], Additive[Point[P, _]]): Point[P, A] = segment match
    case LineSegment(a, b) =>
      val v = b ^-^ a
      val u = a ^-^ p
      val t = -dot(v, u) / quadrance(v)
      if t < zero then a
      else if t > one then a ^+^ v
      else a ^+^ (t *^ v)

end LineSegment