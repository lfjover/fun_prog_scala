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
        else {                            // the element was a right parenteses
          if (nLeft == 0) false           // there isn't a left parenteses to balance it out
          else isBalanced(parensList.tail, nLeft - 1)
        }
      }

      val parentesesList = for(elem <- chars; if (elem=='('|elem==')')) yield elem

      isBalanced(parentesesList, 0)
    }
  
  /**
   * Exercise 3
   */
    def countChange(money: Int, coins: List[Int]): Int = ???
  }
