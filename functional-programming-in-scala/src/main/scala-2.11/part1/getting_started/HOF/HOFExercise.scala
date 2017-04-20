package part1.getting_started.HOF

import scala.annotation.tailrec


object HOFExercise extends App {


  def ex2_1_fibonacci(n: Int): Int = {
    @tailrec
    def go(previous: Int, current: Int, n: Int): Int = {
      if (n <= 0)
        current
      else go(current, previous + current, n - 1)
    }

    go(0, 1, n)
  }

  println(ex2_1_fibonacci(0))
  println(ex2_1_fibonacci(1))
  println(ex2_1_fibonacci(2))
  println(ex2_1_fibonacci(3))
  println(ex2_1_fibonacci(4))
  println(ex2_1_fibonacci(5))
  println(ex2_1_fibonacci(6))
  println(ex2_1_fibonacci(36))
}
