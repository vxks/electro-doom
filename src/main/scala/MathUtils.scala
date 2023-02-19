object MathUtils {
  extension [T](numericT: Numeric[T])
    def bimap[U](tu: T => U)(ut: U => T): Numeric[U] = new Numeric[U]:
      def plus(x: U, y: U): U                 = tu(numericT.plus(ut(x), ut(y)))
      def minus(x: U, y: U): U                = tu(numericT.minus(ut(x), ut(y)))
      def times(x: U, y: U): U                = tu(numericT.times(ut(x), ut(y)))
      def negate(x: U): U                     = tu(numericT.negate(ut(x)))
      def fromInt(x: Int): U                  = tu(numericT.fromInt(x))
      def parseString(str: String): Option[U] = numericT.parseString(str).map(tu)
      def toInt(x: U): Int                    = numericT.toInt(ut(x))
      def toLong(x: U): Long                  = numericT.toLong(ut(x))
      def toFloat(x: U): Float                = numericT.toFloat(ut(x))
      def toDouble(x: U): Double              = numericT.toDouble(ut(x))
      def compare(x: U, y: U): Int            = numericT.compare(ut(x), ut(y))
}
