object Exercises {

  def product(f: Int => Int) (a: Int, b: Int) : Int =
			if (a > b) 1
			else f(a) * product(f)(a+1, b)
                                                  //> product: (f: Int => Int)(a: Int, b: Int)Int
  product(x => x * x)(3, 4)                       //> res0: Int = 144
  
  def factorial(n: Int) : Int =
  	product(x => x)(1,n)                      //> factorial: (n: Int)Int
  
  factorial(4)                                    //> res1: Int = 24
  
  def mapReduce(f: Int => Int) (g: (Int, Int) => Int) (a: Int, b: Int, zero: Int) : Int =
		if (a > b) zero
		else g(f(a), mapReduce(f)(g)(a+1, b, zero))
                                                  //> mapReduce: (f: Int => Int)(g: (Int, Int) => Int)(a: Int, b: Int, zero: Int)I
                                                  //| nt
 	mapReduce(x=>x)((x,y)=>(x+y))(1,5,0)      //> res2: Int = 15
		
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
	}
	
	def test1 = new Rational(1,3)             //> test1: => Exercises.Rational
	test1.neg                                 //> res3: Exercises.Rational = 1/-3
	
	def test2 = new Rational(5,7)             //> test2: => Exercises.Rational
	test1.sub(test2)                          //> res4: Exercises.Rational = 8/-21
	
	def test3 = new Rational(3,2)             //> test3: => Exercises.Rational
	
	test1.sub(test2).sub(test3)               //> res5: Exercises.Rational = -79/42
	
	test2.add(test2).add(test2)               //> res6: Exercises.Rational = 15/7
	
	// removing infix notation, still legal syntax:
	test2.less(test2)                         //> res7: Boolean = false
	test2 less test2                          //> res8: Boolean = false
	
	// using < operator
	test2 < test2                             //> res9: Boolean = false
	
	
}