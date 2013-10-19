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
  def pascal(c: Int, r: Int): Int = {
    if (c == 0 || c == r) 1
    else pascal(c, r - 1) + pascal(c - 1, r - 1)
  }

  /**
   * Exercise 2
   */
  def balance(chars: List[Char]): Boolean = {
    def balanceAccumulated(chars: List[Char], acc: Int) : Boolean =
      if (acc < 0) false
      else if (chars.isEmpty) acc == 0
      else if (chars.head == '(') balanceAccumulated(chars.tail, acc + 1)
      else if (chars.head == ')') balanceAccumulated(chars.tail, acc - 1)
      else balanceAccumulated(chars.tail, acc)

    balanceAccumulated(chars, 0)
  }

  /**
   * Exercise 3
   */
  def countChange(money: Int, coins: List[Int]): Int = {
    def countChangeSingleAllowed(money: Int, coins: List[Int], allowed: Boolean) : Int = 
      if (money <= 0 || coins.isEmpty) 0
      else if (money % coins.head == 0 && allowed) 1 + countChangeSingleAllowed(money, coins, false)
      else countChangeSingleAllowed(money - coins.head, coins, false) + countChange(money, coins.tail)
    countChangeSingleAllowed(money, coins, true)
  }
}
