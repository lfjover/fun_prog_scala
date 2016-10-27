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
      if (c==0|r==0|c==r) 1
      else pascal(c-1, r-1) + pascal(c, r-1)
    }

  /**
   * Exercise 2
   */
    def balance(chars: List[Char]): Boolean = {


      def isBalanced(parensList: List[Char], nLeft: Int): Boolean = {
        if (parensList.isEmpty) {
          if (nLeft == 0) true else false
        }
        else if (parensList.head == '(') isBalanced(parensList.tail, nLeft + 1)
        else {                            // the element was a right parentheses
          if (nLeft == 0) false           // there isn't a left parentheses to balance it out
          else isBalanced(parensList.tail, nLeft - 1)
        }
      }

      val parenthesesList = for(elem <- chars; if (elem=='('|elem==')')) yield elem

      isBalanced(parenthesesList, 0)
    }
  
  /**
   * Exercise 3
   */
    def countChange(money: Int, coins: List[Int]): Int = {
      if (money == 0) 0
      else if (coins.isEmpty) 0
      else {
        val n = (0 to money/coins(0)).toList
        if (money%coins(0) == 0) 1 + n.map(x => countChange(money - x*coins(0), coins.tail)).sum
        else n.map(x => countChange(money - x*coins(0), coins.tail)).sum
      }

    }
  }
