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
   * Exercise 1: Note p(c,r) = p(c-1,r-1) + p(c,r-1)
   */
  def pascal(c: Int, r: Int): Int = (c,r) match {
    case (_,0)             => 1
    case (0,_)             => pascal(0,r-1)
    case (cp,_) if cp == r => pascal(cp-1,r-1)
    case _                 => pascal(c-1,r-1) + pascal(c,r-1)
  }
  
  /**
   * Exercise 2
   */
  def balance(chars: List[Char]): Boolean = {
    def balanceAux(open: Int, remaining: List[Char]): Boolean = remaining match {
      case Nil => open == 0
      case '(' :: xs => balanceAux(open+1, xs)
      case ')' :: xs => open > 0 && balanceAux(open-1, xs)
      case _ :: xs   => balanceAux(open, xs)
    }
    balanceAux(0, chars)
  }
  
  /**
   * Exercise 3
   */
  def countChange(money: Int, coins: List[Int]): Int = (money, coins) match {
    case (0, _)          => 1
    case (_, Nil)        => 0
    case (m, _) if m < 0 => 0
    case (_, x :: xs)    => countChange(money - x, coins) + countChange(money, xs)
  }
}
