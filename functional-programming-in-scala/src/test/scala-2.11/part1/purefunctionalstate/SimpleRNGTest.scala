package part1.purefunctionalstate

import org.scalatest.FunSuite


class SimpleRNGTest extends FunSuite {

  import RNG._

  test("testNextInt") {
    val expected1 = SimpleRNG(10).nextInt._1
    val expected2 = SimpleRNG(10).nextInt._1
    assertResult(true)(expected1 == expected2)
  }

  test("get random list of size count") {
    val rng = SimpleRNG(10)
    val rngk = SimpleRNG(10)
    val expected1:  (List[Int], RNG) = ints(4)(rng.nextInt._2)
    println(expected1._1.mkString(","))

    assertResult(4)(expected1._1.size)
    assertResult(false)(5 == expected1._1.size)
    assertResult(false)(expected1._1.head.equals(expected1._1.tail.head))
  }

  test("get random double between 0 and less than 1") {
    val rng = SimpleRNG(10)
    val rndDouble = double(rng)
    assertResult(true)(rndDouble._1 > 0 && rndDouble._1 < 1 )
  }

  test("get random double with map") {
    val rng = SimpleRNG(10)
    val rndDouble = double2(rng)
    assertResult(true)(rndDouble._1 > 0 && rndDouble._1 < 1 )
  }

}
