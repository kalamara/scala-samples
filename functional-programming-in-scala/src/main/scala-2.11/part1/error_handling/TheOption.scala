package part1.error_handling

sealed trait TheOption[+A] {
  def map[B](f: A => B): TheOption[B] = this match {
    case TheNone => TheNone
    case TheSome(a) => TheSome(f(a))
  }

  def map2[A, B, C](a: TheOption[A], b: TheOption[B])(f: (A, B) => C): TheOption[C] =
    (a, b) match {
      case (TheNone, _) => TheNone
      case (_, TheNone) => TheNone
      case (TheSome(x), TheSome(y)) => TheSome(f(x, y))
    }


  def flatMap[B](f: A => TheOption[B]): TheOption[B] =
    map(f) getOrElse TheNone

  def flatMap2[B](f: A => TheOption[B]): TheOption[B] = this match {
    case _ => TheNone
    case TheSome(a) => f(a)
  }

  def getOrElse[B >: A](default: => B): B = this match {
    case TheNone => default
    case TheSome(a) => a
  }

  def orElse[B >: A](ob: => TheOption[B]): TheOption[B] = this match {
    case TheNone => ob
    case TheSome(a) => this
  }

  def filter(f: (A) => Boolean): TheOption[A] = this match {
    case TheSome(a) if (f(a)) => this
    case _ => TheNone
  }
}

case class TheSome[+A](get: A) extends TheOption[A]

case object TheNone extends TheOption[Nothing]