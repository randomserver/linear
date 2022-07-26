package linear

import cats._
import cats.syntax.{given, *}
import Numeric.Implicits.{given, *}
import Fractional.Implicits.{given, *}


trait R1[P[_]]:
  extension [B](p: P[B])
    def x: B

trait R2[P[_]: R1]:
  extension [B](p: P[B])
    def y: B
    
trait R3[P[_]: R2]:
  extension [B](p: P[B])
    def z: B