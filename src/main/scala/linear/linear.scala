package se.randomserver
package linear

import linear.{R1, R2}

import cats.*
import cats.syntax.{*, given}

import scala.math.Fractional.Implicits.{*, given}
import scala.math.Numeric.Implicits.{*, given}


trait R1[P[_]]:
  extension [B](p: P[B])
    def x: B

trait R2[P[_]: R1]:
  extension [B](p: P[B])
    def y: B
    
trait R3[P[_]: R2]:
  extension [B](p: P[B])
    def z: B