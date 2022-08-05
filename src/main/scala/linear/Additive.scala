package se.randomserver
package linear

import cats.Apply

import scala.annotation.targetName
import scala.math.Fractional.Implicits.{*, given}
import scala.math.Numeric.Implicits.{*, given}

object Additive:
  def apply[P[_]](using A: Additive[P]): Additive[P] = A

end Additive

trait Additive[P[_]: Apply]:
  def zero[B: Numeric]: P[B]
  extension [B: Numeric](p: P[B])
    @targetName("add")
    def ^+^(o: P[B]): P[B] = Apply[P].map2(p, o) {
      case (aa, bb) => aa + bb
    }
    @targetName("sub")
    def ^-^(o: P[B]): P[B] = p ^+^ o.neg
    def neg: P[B] = Apply[P].map(p)(b => -b)
    @targetName("scalarProduct")
    def ^*(s: B): P[B] = Apply[P].map(p)(_ * s)
  extension [B: Fractional](p: P[B])
    @targetName("scalarDivision")
    def ^/(s: B): P[B] = Apply[P].map(p)(_ / s)
end Additive



