object SquareRootApproximation {

  def sqrt(x: Double) = {
    def sqrtIter(guess: Double): Double =
      if (isGoodEnough(guess)) guess
      else sqrtIter(improve(guess))

    def isGoodEnough(guess: Double) = abs(guess * guess - x) / x < 0.01

    def improve(guess: Double) = (guess + x / guess) / 2

    def abs(x: Double) = if (x > 0) x else -x

    sqrtIter(1.0)
  }                                               //> sqrt: (x: Double)Double
  
  sqrt(0.001)                                     //> res0: Double = 0.031642015868650786
  sqrt(1e-20)                                     //> res1: Double = 1.0020750635502768E-10
  sqrt(1e20)                                      //> res2: Double = 1.002075063550277E10
  sqrt(1e50)                                      //> res3: Double = 1.000873029120681E25

}