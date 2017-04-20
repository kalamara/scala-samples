package part1.getting_started.polymorphic

import scala.annotation.tailrec


object PolymorphicExample extends App {

  def findFirst1(ss: Array[String], key: String): Int = {
    @tailrec
    def loop(n: Int): Int = {
      if (n >= ss.length) -1
      else if (ss(n) == key) n
      else loop(n + 1)
    }

    loop(0)
  }

  def findFirstPolymorphic[A](p: A => Boolean)(implicit ss: Array[A]): Int = {
    @tailrec
    def loop(n: Int): Int = {
      if (n >= ss.length) -1
      else if (p(ss(n))) n
      else loop(n + 1)
    }

    loop(0)
  }


  val myArr = Array("1", "2", "3")//.find(e => e == "2")
  println(findFirst1(myArr, "2"))



  implicit val myArrA = Array(1, 2, 3)
  /* find the array implicitly */
  println(findFirstPolymorphic( (e: Int) => e == 1 ) )

}
