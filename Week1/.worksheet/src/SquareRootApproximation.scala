object SquareRootApproximation {;import org.scalaide.worksheet.runtime.library.WorksheetSupport._; def main(args: Array[String])=$execute{;$skip(380); 

  def sqrt(x: Double) = {
    def sqrtIter(guess: Double): Double =
      if (isGoodEnough(guess)) guess
      else sqrtIter(improve(guess))

    def isGoodEnough(guess: Double) = abs(guess * guess - x) / x < 0.01

    def improve(guess: Double) = (guess + x / guess) / 2

    def abs(x: Double) = if (x > 0) x else -x

    sqrtIter(1.0)
  };System.out.println("""sqrt: (x: Double)Double""");$skip(18); val res$0 = 
  
  sqrt(0.001);System.out.println("""res0: Double = """ + $show(res$0));$skip(14); val res$1 = 
  sqrt(1e-20);System.out.println("""res1: Double = """ + $show(res$1));$skip(13); val res$2 = 
  sqrt(1e20);System.out.println("""res2: Double = """ + $show(res$2));$skip(13); val res$3 = 
  sqrt(1e50);System.out.println("""res3: Double = """ + $show(res$3))}

}
