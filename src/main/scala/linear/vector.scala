package linear.vector
import Numeric.Implicits.{given, *}
import Fractional.Implicits.{given, *}
import cats.Apply

trait Additive[P[_]: Apply]:
  extension [B: Numeric](p: P[B])
    def +(o: P[B]): P[B] = Apply[P].map2(p, o) {
      case (aa, bb) => aa + bb
    }
    def -(o: P[B]): P[B] = p + o.neg
    def zero: P[B]
    def neg: P[B] = Apply[P].map(p)(b => -b)


