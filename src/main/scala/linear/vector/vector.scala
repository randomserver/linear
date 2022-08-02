package se.randomserver
package linear.vector

import linear.vector.Additive

import cats.Apply

import scala.annotation.targetName
import scala.math.Fractional.Implicits.{*, given}
import scala.math.Numeric.Implicits.{*, given}

object Additive:
  def apply[P[_]](using A: Additive[P]): Additive[P] = A

trait Additive[P[_]: Apply]:
  def zero[B: Numeric]: P[B]
  extension [B: Numeric](p: P[B])
    def +(o: P[B]): P[B] = Apply[P].map2(p, o) {
      case (aa, bb) => aa + bb
    }
    def -(o: P[B]): P[B] = p + o.neg
    def neg: P[B] = Apply[P].map(p)(b => -b)


