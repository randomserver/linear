package se.randomserver
package geometry

trait IsIntersectableWith[P1, P2] extends HasIntersectionWith[P1, P2]  {
  type Intersection
  def intersect(a: P1, b: P2): Option[Intersection]
}

object IsIntersectableWith:
  def intersect[P1, P2](a: P1, b: P2)(using is: IsIntersectableWith[P1, P2]): Option[is.Intersection] = is.intersect(a ,b)
end IsIntersectableWith
