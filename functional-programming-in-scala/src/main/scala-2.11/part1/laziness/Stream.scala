package part1.laziness

import scala.annotation.tailrec

sealed trait Stream[+A] {
  def headOption: Option[A] = this match {
    case Empty => None
    case Cons(h, t) => Some(h())
  }

  def toList: List[A] = {
    @tailrec
    def go(s: Stream[A], acc: List[A]):List[A] = s match {
      case Cons(h, t) => go(t(), h()::acc)
      case _ => acc
    }
    go(this, List()).reverse
  }

  def take(n: Int): Stream[A] = this match {
    case Cons(h, t) if n > 1 => Stream.cons(h(), t().take(n - 1))
    case Cons(h, _) if n == 1 => Stream.cons(h(), Stream.empty)
    case _ => Empty
  }

  @tailrec
  final def drop(n: Int): Stream[A] = this match {
    case Cons(_, t) if n > 0 => t().drop(n - 1)
    case _ => this
  }

  def takeWhile(p: A => Boolean): Stream[A] =/* this match {
    case Cons(h, t) if p(h()) => Stream.cons(h(), t().takeWhile(p))
    case _ => Empty
  }*/
  foldRight(Stream.empty[A])(
    (h,t) => {
    if(p(h))
      Stream.cons(h,t)
    else
      Stream.empty
    }
  )

  def foldRight[B](z: => B)(f: (A, => B) => B): B = this match {
    case Cons(h, t) => f(h(), t().foldRight(z)(f))
    case _ => z
  }

  def exists(p: A => Boolean): Boolean =
    foldRight(false)((a, b) => p(a) || b)

  def forAll(p: A => Boolean): Boolean =
    foldRight(true)((a, b) => p(a) && b)

  def map[B](f: A => B): Stream[B] =
    foldRight(Stream.empty[B])((h,t) => Stream.cons(f(h), t))

  def filter(f: A => Boolean): Stream[A] =
    foldRight(Stream.empty[A])(
      (h,t) => {
        if(f(h))
          Stream.cons(h,t)
        else
          t
      }
    )

  def append[B >: A](s: Stream[B]): Stream[B] =
    foldRight(s)((h,t) => Stream.cons(h,t))


}

case object Empty extends Stream[Nothing]

case class Cons[+A](head: () => A,
                      tail: () => Stream[A]) extends Stream[A]
object Stream {
  def cons[A] (hd: => A,
               tl: => Stream[A]): Stream[A] = {
    lazy val h = hd
    lazy val t = tl
    Cons(() => h, () => t)
  }
  def empty[A] : Stream[A] = Empty

  def apply[A](as: A*): Stream[A] = {
    if (as.isEmpty)
      empty
    else
      cons(as.head, apply(as.tail: _*))
  }

  val ones: Stream[Int] = cons(1, ones)

  def const[A](a: A): Stream[A] = cons(a, const(a))

  def from(a: Int): Stream[Int] = cons(a, from(a + 1))

  def fibs: Stream[Int] = {
    def go(f0: Int, f1: Int): Stream[Int] =
      cons(f0, go(f1, f0 + f1))
    go(0,1)
  }

}
