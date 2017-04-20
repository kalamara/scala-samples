package part1.purefunctionalstate


trait RNG {
  def nextInt: (Int, RNG)
}

object RNG {

  case class SimpleRNG(seed: Long) extends RNG {
    override def nextInt: (Int, RNG) = {
      val newSeed: Long = (seed * 0x5DEECE66DL + 0xBL) & 0xFFFFFFFFFFFFL
      val nextRNG: RNG = SimpleRNG(newSeed)
      val n = (newSeed >>> 16).toInt

      (n, nextRNG)
    }
  }

  def nonNegativeInt(rng: RNG): (Int, RNG) = {
    val (num, state) = rng.nextInt
    (if (num < 0) -num + 1 else num, state)
  }

  def double(rng: RNG): (Double, RNG) = {
    val (num, state) = nonNegativeInt(rng)
    (num / (Int.MaxValue.toDouble + 1), state)
  }

  def intDouble(rng: RNG): ((Int, Double), RNG) = {
    val intRnd = rng.nextInt
    val doubleRnd = double(intRnd._2)

    ((intRnd._1, doubleRnd._1), doubleRnd._2)
  }

  def doubleInt(rng: RNG): ((Double, Int), RNG) = ???

  def double3(rng: RNG): ((Double, Double, Double), RNG) = ???


  def ints(count: Int)(rng: RNG): (List[Int], RNG) = {
    def go(count: Int, li: List[Int])(rng1: RNG): (List[Int], RNG) = {
      if (count <= 0)
        (li, rng1)
      else {
        go(count - 1, li :+ rng1.nextInt._1)(rng1.nextInt._2)
      }
    }

    go(count, List())(rng)
  }

  type Rand[+A] = RNG => (A, RNG)

  val int: Rand[Int] = _.nextInt

  def unit[A](a: A): Rand[A] =
    rng => (a, rng)

  def map[A, B](s: Rand[A])(f: A => B): Rand[B] =
    rng => {
      val (a, rng2) = s(rng)
      (f(a), rng2)
    }

  def nonNegativeEven: Rand[Int] =
    map(nonNegativeInt)(i => i - i % 2)

  def double2: Rand[Double] =
    map(nonNegativeInt)(_ / (Int.MaxValue.toDouble + 1))


  def map2[A, B, C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] =
    rng => {
      val (n1, r1) = ra(rng)
      val (n2, r2) = rb(r1)

      (f(n1, n2), r2)
    }

  def both[A,B](ra: Rand[A], rb: Rand[B]): Rand[(A,B)] =
    map2(ra, rb)((_, _))

  val randIntDouble: Rand[(Int, Double)] =
    both(int, double)

  def flatMap[A,B](f: Rand[A])(g: A => Rand[B]): Rand[B] =
    rng => {
      val (n1, r1) = f(rng)
      g(n1)(r1)
    }

  def nonNegativeLessThan(n: Int): Rand[Int] = {
    flatMap(nonNegativeInt) { i =>
      val mod = i % n
      if (i + (n-1) - mod >= 0) unit(mod) else nonNegativeLessThan(n)
    }
  }

  def rollDie: Rand[Int] = map(nonNegativeLessThan(6))(_ + 1)



  case class State[S, +A](run: S => (A, S))

}

