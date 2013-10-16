object Exercises {;import org.scalaide.worksheet.runtime.library.WorksheetSupport._; def main(args: Array[String])=$execute{;$skip(123); 

  def product(f: Int => Int) (a: Int, b: Int) : Int =
			if (a > b) 1
			else f(a) * product(f)(a+1, b);System.out.println("""product: (f: Int => Int)(a: Int, b: Int)Int""");$skip(28); val res$0 = 
  product(x => x * x)(3, 4);System.out.println("""res0: Int = """ + $show(res$0));$skip(59); 
  
  def factorial(n: Int) : Int =
  	product(x => x)(1,n);System.out.println("""factorial: (n: Int)Int""");$skip(18); val res$1 = 
  
  factorial(4);System.out.println("""res1: Int = """ + $show(res$1));$skip(157); 
  
  def mapReduce(f: Int => Int) (g: (Int, Int) => Int) (a: Int, b: Int, zero: Int) : Int =
		if (a > b) zero
		else g(f(a), mapReduce(f)(g)(a+1, b, zero));System.out.println("""mapReduce: (f: Int => Int)(g: (Int, Int) => Int)(a: Int, b: Int, zero: Int)Int""");$skip(39); val res$2 = 
 	mapReduce(x=>x)((x,y)=>(x+y))(1,5,0)
		
	//default constructor executes class body with x,y values passed in
	class Rational(x: Int, y: Int)
	{
		require(y != 0, "Denominator must not be equal to zero") // checking preconditions (there is also 'assert' keyword we can use in unit tests to check postcondition
		
		//additional constructor
		def this(x: Int) = this(x,1)
		
		private def gcd(a: Int, b: Int): Int = if(b==0) a else gcd(b, a%b)
		private val g = gcd(x,y) // value type = right hand side evaluated at time of definition, so we don't recompute gcd(x,y)
		
		def numerator = x / g
		def denominator = y / g
		
		def less(that: Rational) = numerator * that.denominator < that.numerator * denominator
		def <(that: Rational) = this less that
		
		def max(that: Rational) = if(this.less(that)) that else this
		
		def neg() = new Rational(-1 * numerator, denominator)
		
		def add(that: Rational) = sub(that.neg)
		
		def sub(that: Rational) = new Rational(numerator * that.denominator - that.numerator * denominator, denominator * that.denominator)
		
		override def toString = numerator  + "/" + denominator
	};System.out.println("""res2: Int = """ + $show(res$2));$skip(1117); 
	
	def test1 = new Rational(1,3);System.out.println("""test1: => Exercises.Rational""");$skip(11); val res$3 = 
	test1.neg;System.out.println("""res3: Exercises.Rational = """ + $show(res$3));$skip(33); 
	
	def test2 = new Rational(5,7);System.out.println("""test2: => Exercises.Rational""");$skip(18); val res$4 = 
	test1.sub(test2);System.out.println("""res4: Exercises.Rational = """ + $show(res$4));$skip(33); 
	
	def test3 = new Rational(3,2);System.out.println("""test3: => Exercises.Rational""");$skip(31); val res$5 = 
	
	test1.sub(test2).sub(test3);System.out.println("""res5: Exercises.Rational = """ + $show(res$5));$skip(31); val res$6 = 
	
	test2.add(test2).add(test2);System.out.println("""res6: Exercises.Rational = """ + $show(res$6));$skip(70); val res$7 = 
	
	// removing infix notation, still legal syntax:
	test2.less(test2);System.out.println("""res7: Boolean = """ + $show(res$7));$skip(18); val res$8 = 
	test2 less test2;System.out.println("""res8: Boolean = """ + $show(res$8));$skip(38); val res$9 = 
	
	// using < operator
	test2 < test2;System.out.println("""res9: Boolean = """ + $show(res$9))}
	
	
}
