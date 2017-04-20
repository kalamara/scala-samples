package part1.functional_data_structures

import scala.annotation.tailrec
import scala.collection.concurrent.RDCSS_Descriptor
import scala.collection.mutable.ArrayBuffer

sealed trait MyList[+A]

case object Nil extends MyList[Nothing]

case class Cons[+A](head: A, tail: MyList[A]) extends MyList[A]

object MyList {


  /** A function that uses
    * pattern matching to
    * add up a list of integers. */
  def sum(ints: MyList[Int]): Int =
    ints match {
      /** The sum of the empty list is 0. */
      case Nil => 0
      case Cons(x, xs) => x + sum(xs)
    }

  def product(ds: MyList[Double]): Double =
    ds match {
      case Nil => 1.0
      case Cons(0.0, _) => 0.0
      case Cons(x, xs) => x * product(xs)
    }

  /** Another data
    * constructor,
    * representing
    * nonempty lists.
    * Note that tail is
    * another List[A] ,
    * which may be Nil
    * or another Cons . */
  def apply[A](as: A*): MyList[A] = /*Variadic function syntax.*/
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))

  /** exercise 3.2 **/
  def myTail[A](l: MyList[A]): MyList[A] =
    l match {
      case Nil => Nil
      case Cons(_, t) => t
    }

  def myHead[A](l: MyList[A]): A =
    l match {
      case Cons(h, _) => h
      case Nil => sys.error("head of empty list")
    }

  /** exercise 3.3 **/
  def setHead[A](h: A, l: MyList[A]): MyList[A] =
    l match {
      case Nil => Nil
      case Cons(_, t) => Cons(h, t)
    }

  /** exercise 3.4 **/
  @tailrec
  def drop[A](i: Int, l: MyList[A]): MyList[A] = {
    if (i <= 0) l
    else l match {
      case Nil => Nil
      case Cons(_, t) => drop(i - 1, t)
    }
  }

  /** exercise 3.5 */
  @tailrec
  def dropWhile[A](l: MyList[A], f: A => Boolean): MyList[A] = {
    l match {
      case Cons(h, t) if f(h) => dropWhile(t, f)
      case _ => l
    }
  }

  /** exercise 3.6 */
  def init[A](l: MyList[A]): MyList[A] = {
    l match {
      case Cons(_, Nil) => Nil
      case Cons(h, t) => Cons(h, init(t))
      case Nil => Nil
    }
  }

  def foldRight[A, B](as: MyList[A], z: B)(f: (A, B) => B): B = {
    as match {
      case Nil => z
      case Cons(x, xs) => {
        f(x, foldRight(xs, z)(f))
      }
    }
  }

  /** exercise 3.7 apparently we cannot stop recursion with current implementation of foldRight */
  def product2(li: MyList[Double]): Double = {
    foldRight(li, 1.0)((x, y) => if (x == 0 || y == 0) 0 else x * y)
  }

  /** exercise 3.10 */
  @tailrec
  def foldLeft[A, B](as: MyList[A], z: B)(f: (B, A) => B): B = {
    as match {
      case Nil => z
      case Cons(x, xs) => foldLeft(xs, f(z, x))(f)
    }
  }

  /** exercise 3.9 - 3.11 */
  def length[A](l: MyList[A]): Int = {
    //foldRight(l, 0)((x, y:Int) => 1 + y)
    foldLeft(l, 0)((x: Int, y) => 1 + x)
  }

  /** exercise 3.9 second version */
  def length2[A](l: MyList[A]): Int = {
    foldRight(l, 0)((x, z) => 1 + z)
  }

  /** exercise 3.12 */
  def reverse[A](l: MyList[A]): MyList[A] = foldLeft(l, MyList[A]())((acc, h) => Cons(h, acc))

  /** exercise 3.12 v2 */
  def reverse2[A](li: MyList[A]): MyList[A] = {
    def go(li: MyList[A]): ArrayBuffer[A] = {
      val buf = ArrayBuffer.empty[A]
      li match {
        case Nil => buf
        case Cons(x, xs) => foldRight(Cons(x, xs), buf)((x, buf) => buf += x)
      }
    }
    MyList(go(li): _*)
  }


  def append[A](a1: MyList[A], a2: MyList[A]): MyList[A] =
    a1 match {
      case Nil => a2
      case Cons(h, t) => Cons(h, append(t, a2))
    }

  def appendFoldRight[A](a1: MyList[A], a2: MyList[A]): MyList[A] =
    a1 match {
      case Nil => a2
      case Cons(h, t) => foldRight(a1, a2)((x, z) => Cons(x, z))
    }

  def appendViaFoldRight[A](l: MyList[A], r: MyList[A]): MyList[A] =
    foldRight(l, r)(Cons(_, _))

  def appendFoldLeft2[A](a1: MyList[A], a2: MyList[A]): MyList[A] =
    a1 match {
      case Nil => a2
      case Cons(h, t) => foldLeft(a1, a2)((z, x) => Cons(x, z))
    }

  def appendFoldLeft[A](a1: MyList[A], a2: MyList[A]): MyList[A] =
    foldLeft(a1, a2)((z, x) => Cons(x, z))

  /** exercise 3.16 */
  def increment(li: MyList[Int], num: Int): MyList[Int] =
    foldRight(li, MyList[Int]())((x, z) => Cons(x + num, z))

  //reverse(foldLeft(li,MyList[Int]())((z, x) => Cons(x+num, z)))

  /** exercise 3.16 */
  def double2string(li: MyList[Double]): MyList[String] =
    foldRight(li, MyList[String]())((x, z) => Cons(x.toString, z))

  //reverse(foldLeft(li,MyList[Int]())((z, x) => Cons(x+num, z)))

  /** exercise 3.18 */
  def map[A, B](li: MyList[A])(f: A => B): MyList[B] = {
    foldRight(li, MyList[B]())((x: A, z: MyList[B]) => Cons(f(x), z))
  }

  /** exercise 3.19 */
  def filter[A](li: MyList[A])(f: A => Boolean): MyList[A] =
    foldRight(li, MyList[A]())((h: A, t: MyList[A]) => if (f(h)) Cons(h, t) else t)

  /** exercise 3.20 */
  def flatMap[A, B](li: MyList[A])(f: A => MyList[B]): MyList[B] = {
    foldRight(li, MyList[B]())((x: A, z: MyList[B]) => append(f(x), z))
  }

  /** exercise 3.21 */
  def filter2[A](li: MyList[A])(f: A => Boolean): MyList[A] =
    flatMap(li)((x: A) => if (f(x)) MyList(x) else Nil)

  /** exercise 3.21 */
  def zipWithAddInt(a: MyList[Int], b: MyList[Int]): MyList[Int] =
    (a, b) match {
      case (Nil, Nil) => Nil
      case (Nil, _) => b
      case (_, Nil) => a
      case (Cons(h1, t1), Cons(h2, t2)) => Cons(h1 + h2, zipWithAddInt(t1, t2))
    }

  def zipWithAddInt2(a: MyList[Int], b: MyList[Int]): MyList[Int] =
    (a, b) match {
      case (Nil, _) => Nil
      case (_, Nil) => Nil
      case (Cons(h1, t1), Cons(h2, t2)) => Cons(h1 + h2, zipWithAddInt(t1, t2))
    }

  /** exercise 3.22 */
  def zipWith[A, B, C](a: MyList[A], b: MyList[B])(f: (A, B) => C): MyList[C] =
    (a, b) match {
      case (Nil, _) => Nil
      case (_, Nil) => Nil
      case (Cons(h1, t1), Cons(h2, t2)) => Cons(f(h1, h2), zipWith(t1, t2)(f))
    }

  /** exercise 3.24 */
  def hasSubsequence[A](sup: MyList[A], sub: MyList[A]): Boolean = {
    @tailrec
    def go[A](sup: MyList[A], sub: MyList[A]): Boolean =
      (sup, sub) match {
        case (Nil, _) => false
        case (_, Nil) => true
        case (Cons(h1, t1), Cons(h2, t2)) =>
          if (h1 != h2)
            (go(t1, sub))
          else go(t1, t2)
      }

    go(sup, sub)
  }

}