package part1.getting_started.following_types

object TypesExercise extends App {
  def curry[A,B,C](f: (A, B) => C): A => (B => C) =
    (a:A) => (b:B) => f(a, b)



  def f(s: String, i: Int): Boolean = s.length == i

  f("lol", 3)

  curry(f)("lol")(2)

  def uncurry[A,B,C](f: A => (B => C)): (A, B) => C =
    (a, b) => f(a)(b)

  def compose[A,B,C](f: B => C, g: A => B): A => C =
    (a: A) => f(g(a))

  def g(x: String): Array[Char] = x.toCharArray

  def f(y: Array[Char]): Int = y.length

  compose(f, g)("lol")

}


