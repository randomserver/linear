package se.randomserver
package geometry

trait HasIntersectionWith[P1, P2] {
  def intersects(p1: P1, p2: P2): Boolean
}

object HasInterSectionWith {
  def intersects[P1, P2] = (p1: P1, p2: P2) => (I: HasIntersectionWith[P1, P2]) ?=> I.intersects(p1, p2)
}