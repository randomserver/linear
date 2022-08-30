package se.randomserver
package linear

trait LinearIntegral[T] extends Integral[T] {
  def sqrt(x: T): T
}

object LinearIntegral {
  trait DoubleIsConflicted extends Numeric[Double] {
    def plus(x: Double, y: Double): Double = x + y

    def minus(x: Double, y: Double): Double = x - y

    def times(x: Double, y: Double): Double = x * y

    def negate(x: Double): Double = -x

    def fromInt(x: Int): Double = x.toDouble

    def parseString(str: String): Option[Double] = str.toDoubleOption

    def toInt(x: Double): Int = x.intValue

    def toLong(x: Double): Long = x.longValue

    def toFloat(x: Double): Float = x.floatValue

    def toDouble(x: Double): Double = x.doubleValue
  }

  trait FloatIsConflicted extends Numeric[Float] {
    def plus(x: Float, y: Float): Float = x + y

    def minus(x: Float, y: Float): Float = x - y

    def times(x: Float, y: Float): Float = x * y

    def negate(x: Float): Float = -x

    def fromInt(x: Int): Float = x.toFloat

    def parseString(str: String): Option[Float] = str.toFloatOption

    def toInt(x: Float): Int = x.intValue

    def toLong(x: Float): Long = x.longValue

    def toFloat(x: Float): Float = x.floatValue

    def toDouble(x: Float): Double = x.doubleValue
  }
  
  
  given LinearIntegral[Int] with Numeric.IntIsIntegral with Ordering.IntOrdering with
    def sqrt(x: Int): Int = math.sqrt(x).round.toInt

  given LinearIntegral[Long] with Numeric.LongIsIntegral with Ordering.LongOrdering with
    def sqrt(x: Long): Long = math.sqrt(x).round.toLong

  given LinearIntegral[Float] with FloatIsConflicted with Ordering.Float.IeeeOrdering with {
    def sqrt(x: Float): Float = math.sqrt(x.toDouble).toFloat

    override def quot(x: Float, y: Float): Float = x / y

    override def rem(x: Float, y: Float): Float = x % y
  }

  given LinearIntegral[Double] with DoubleIsConflicted with Ordering.Double.IeeeOrdering with {
    def sqrt(x: Double): Double = math.sqrt(x)

    override def quot(x: Double, y: Double): Double = x / y

    override def rem(x: Double, y: Double): Double = x % y
  }

  def sqrt[A: LinearIntegral](x: A) = summon[LinearIntegral[A]].sqrt(x)

  export math.Ordering.Implicits.infixOrderingOps
  export math.Integral.Implicits.infixIntegralOps
}
