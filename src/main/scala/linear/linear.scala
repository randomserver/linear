package se.randomserver
package linear

object R1:
  def apply[P[_]](using R: R1[P]): R1[P] = R

  extension[P[_] : R1, B] (p: P[B])
    def x: B = summon[R1[P]].x(p)
end R1

trait R1[P[_]]:
  def x[B](p: P[B]): B

object R2:
  def apply[P[_]](using R: R2[P]): R2[P] = R

  extension[P[_] : R2, B] (p: P[B])
    def y: B = summon[R2[P]].y(p)
end R2

trait R2[P[_]: R1]:
  def y[B](p: P[B]): B


object R3:
  def apply[P[_]](using R: R3[P]): R3[P] = R

  extension[P[_] : R3, B] (p: P[B])
    def z: B = summon[R3[P]].z(p)
end R3

trait R3[P[_]: R1: R2]:
  def z[B](p: P[B]): B

  
  
  
  