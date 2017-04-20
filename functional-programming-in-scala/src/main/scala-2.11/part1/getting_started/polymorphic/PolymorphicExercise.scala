package part1.getting_started.polymorphic

import scala.annotation.tailrec


object PolymorphicExercise extends App {

  val arr = Array(1, 2, 3, 4, 5)

  def isSortedEx[A](ar: Array[A], ord: (A, A) => Boolean): Boolean = {
    @tailrec
    def go(n: Int): Boolean = {
      if (ar.length <= 0) false
      else if (ar.length - 1 == n) true
      else if (ord(ar(n), ar(n + 1))) go(n + 1)
      else false
    }

    go(0)
  }

  println(isSortedEx(arr, (a: Int, b: Int) => a <= b))

}
