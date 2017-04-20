package part1.laziness

import org.scalatest.FunSuite


class StreamTest extends FunSuite {

  /** 5.1 */
  test("toList") {

    val lol: Stream[Int] = Stream.cons(1, Stream.cons(2, Stream.empty))
    val exp: List[Int] = List(1, 2)
    assertResult(exp)(lol.toList)
    assertResult(Nil)(Stream.empty.toList)
  }

  /** 5.2 */
  test("take(n)") {
    val lol: Stream[Int] = Stream.apply(1, 2, 3, 4, 5)
    val exp: List[Int] = List(1, 2, 3)
    assertResult(exp)(lol.take(3).toList)
    assertResult(lol.toList)(lol.take(10).toList)
    assertResult(Nil)(Stream.empty.take(10).toList)
  }

  test("drop(n)") {
    val lol: Stream[Int] = Stream.apply(1, 2, 3, 4, 5)
    val exp: List[Int] = List(3, 4, 5)
    assertResult(exp)(lol.drop(2).toList)
    assertResult(Nil)(lol.drop(10).toList)
    assertResult(Nil)(Stream.empty.take(10).toList)

  }

  /** 5.3 */
  test("takeWhile") {
    val lol: Stream[Int] = Stream.apply(2, 4, 6, 8, 5)
    val exp: List[Int] = List(2, 4, 6, 8)
    assertResult(exp)(lol.takeWhile(_ % 2 == 0).toList)
  }

  /** 5.4 */
  test("forAll") {
    val  lol: Stream[Int] = Stream.apply(2, 4, 6, 8, 5)
    val  evens: Stream[Int] = Stream.apply(2, 4, 6, 8, 0)
    assert(!lol.forAll(_ % 2 == 0))
    assert(evens.forAll(_ % 2 == 0))
  }

  /**5.5*/

  test("map") {
    val  lol: Stream[Int] = Stream.apply(2, 4, 6, 8)
    val  doubles: List[String] = List("2", "4", "6", "8")
    assertResult(doubles)(lol.map(_.toString).toList)

  }

  test("filter") {
    val lol: Stream[Int] = Stream.apply(1, 2, 3, 4, 5, 6, 7, 8)
    val doubles: List[Int] = List(2, 4, 6, 8)
    assertResult(doubles)(lol.filter(_ % 2 == 0).toList)
  }

  test("append") {
    val lol: Stream[Int] = Stream.apply(1, 2, 3, 4, 5, 6, 7, 8)
    val exp: List[Int] = List(1, 2, 3, 4, 5, 6, 7, 8, 9)
    assertResult(exp)(lol.append(Stream.apply(9)).toList)
  }

  /** 5.8 */
  test("const") {
    val exp: List[Int] = List(2,2,2,2)
    assertResult(exp)(Stream.const(2).take(4).toList)
  }

  /** 5.9 */
  test("from") {
    val exp: List[Int] = List(2,3,4,5)
    assertResult(exp)(Stream.from(2).take(4).toList)
  }

  /**5.10*/
  test("fibs"){
    val exp: List[Int] = List(0,1,1,2,3,5,8,13,21)
    assertResult(exp)(Stream.fibs.take(9).toList)
  }

}