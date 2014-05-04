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
    if (r < 0 || c < 0 || c > r) {
      throw new java.util.NoSuchElementException()
    } else {
      if (r == 0 || c == 0 || c == r) {
        1
      } else {
        pascal(c - 1, r - 1) + pascal(c, r - 1)
      }
    }
  /**
   * Exercise 2
   */
  def balance(chars: List[Char]): Boolean = {

    def checker(subs: List[Char], count: Int): Int =
      if (subs.isEmpty) {
        count
      } else if (subs.head == ')') {
        if (count < 1)
          -1
        else
          checker(subs.tail, count - 1)
      } else if (subs.head == '(') {
        checker(subs.tail, count + 1)
      } else {
        checker(subs.tail, count)
      }
    checker(chars, 0) == 0
  }

  /**
   * Exercise 3
   */
  def countChange(money: Int, coins: List[Int]): Int =
    if (coins.isEmpty) { 0 }
    else if (money == 0) {
      1
    } else if (money < 0) {
      0
    } else {
      countChange(money, coins.tail) + countChange(money - coins.head, coins)
    }
}