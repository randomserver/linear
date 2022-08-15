package se.randomserver
package geometry

trait Intersects[P1, P2] {
  def intersects(p1: P1, p2: P2): Boolean
}
