package part1.getting_started

import scala.annotation.tailrec

object MyModule extends App {

  def abs(n: Int): Int =
    if (n < 0) -n
    else n

  private def formatAbs(x: Int) = {
    val msg = "The absolute value of %d is %d"
    msg.format(x, abs(x))
  }

  def factorial(n: Int): Int = {

    @tailrec
    def go(n: Int, acc: Int): Int = {
      if(n <= 0) acc
      else go(n - 1, n * acc)
    }

    go(n, 1)
  }

  def formatResult(name : String, n: Int, f: Int => Int) = {
    val msg = "The %s of %d is %d."
    msg.format(name, n, f(n))
  }


  println(formatAbs(-5))
  println(formatResult("factorial", 3, factorial))
}
