package se.randomserver
package linear

import scala.math.Numeric.DoubleIsFractional

trait Floating[A] extends Fractional[A]:
  def sqrt(a: A): A

object Floating:
  given Floating[Double] with Numeric.DoubleIsFractional with Ordering.Double.IeeeOrdering with
     override def sqrt(a: Double): Double = math.sqrt(a)

  given Floating[Float] with Numeric.FloatIsFractional with Ordering.Float.IeeeOrdering with
    override def sqrt(a: Float): Float = math.sqrt(a.toDouble).toFloat
    
  def sqrt[A: Floating](a: A): A = summon[Floating[A]].sqrt(a)
end Floating