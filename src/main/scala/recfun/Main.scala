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
  def pascal(c: Int, r: Int): Int =
    if (c == 0 || c == r) 1 // top of the triangle is always 1. Likewise, left-hand column is always 1
    else pascal(c - 1, r - 1) + pascal(c, r - 1)


  /**
    * Exercise 2
    */
  def balance(chars: List[Char]): Boolean = {
    def loop(fwdParenCount: Int, bckParenCount: Int, str: List[Char]): Boolean = {
      if (str.isEmpty) fwdParenCount == bckParenCount && /*needs to be more than just parens: */(fwdParenCount + bckParenCount) != chars.length
      else if (str.head == '(') loop(fwdParenCount + 1, bckParenCount, str.tail)
      else if (str.head == ')') loop(fwdParenCount, bckParenCount + 1, str.tail)
      else loop(fwdParenCount, bckParenCount, str.tail)
    }

    loop(0, 0, chars)
  }

  /**
    * Exercise 3
    */
  def countChange(money: Int, coins: List[Int]): Int = {
    if (coins.isEmpty || money < 0) 0
    else if (money == 0) 1
    else {
      val lhs = countChange(money, coins.tail);
      val rhs = countChange(money - coins.head, coins)
      return lhs + rhs
    }
  }
}
