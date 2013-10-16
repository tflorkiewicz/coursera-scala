package recfun
import common._

object Main {
  def main(args: Array[String]) {
    println("Pascal's Triangle")
    for (row <- 0 to 10) {
      for (col <- 0 to row)
        print(pascal(col, row) + " ")
      println()
    }
  }

  /**
   * Exercise 1
   */
  def pascal(c: Int, r: Int): Int =
    {
      if (c == 0 || c == r) 1 else pascal(c - 1, r - 1) + pascal(c, r - 1)
    }

  /**
   * Exercise 2
   */
  def balance(chars: List[Char]): Boolean =
    {
      def balanceIter(charsIter: List[Char], acc: Int): Boolean =
        {
          if (charsIter.isEmpty || acc < 0) acc == 0
          else if ('('.equals(charsIter.head)) balanceIter(charsIter.tail, acc + 1)
          else if (')'.equals(charsIter.head)) balanceIter(charsIter.tail, acc - 1) else balanceIter(charsIter.tail, acc)
        }
      balanceIter(chars, 0)
    }

  /**
   * Exercise 3
   */
  def countChange(money: Int, coins: List[Int]): Int =
    {
      def countChangeIter(moneyIter: Int, coinsIter: List[Int]): Int =
        {
          if (moneyIter == 0) 1
          else if (coinsIter.isEmpty) 0
          else if (moneyIter >= coinsIter.head) countChangeIter(moneyIter, coinsIter.tail) + countChangeIter(moneyIter - coinsIter.head, coinsIter)
          else countChangeIter(moneyIter, coinsIter.tail)
        }

      if (money == 0 || coins.isEmpty) 0
      else countChangeIter(money, coins) 
    }

}
