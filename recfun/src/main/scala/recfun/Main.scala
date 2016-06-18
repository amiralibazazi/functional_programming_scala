package recfun

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
    if (c == 0 || r == c) 1
    else pascal(c-1, r-1) + pascal(c, r-1)
  }
  
  /**
   * Exercise 2
   */
  def balance(chars: List[Char]): Boolean = {

    def rebalance(openCount: Int, chars: List[Char]): Int= {
      if (openCount < 0) -1
      else if (chars.isEmpty) openCount
      else if (chars.head == '(') rebalance(openCount + 1, chars.tail)
      else if (chars.head == ')') rebalance(openCount - 1, chars.tail)
      else rebalance(openCount, chars.tail)
    }

    rebalance(0, chars) == 0
  }
  
  /**
   * Exercise 3
   */
  def countChange(money: Int, coins: List[Int]): Int = {

    def func(remainingMoney: Int, coins: List[Int], combinations: String): Int = {
      if (remainingMoney < 0 || coins.isEmpty) 0
      else if (remainingMoney == 0) 1
      else func(remainingMoney, coins.tail, combinations) +
        func(remainingMoney-coins.head, coins, combinations+"+"+coins.head)
    }

    func(money, coins, "")
  }
}
