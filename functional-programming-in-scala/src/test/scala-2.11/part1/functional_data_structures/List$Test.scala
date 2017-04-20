package part1.functional_data_structures

import org.scalatest.FunSuite
import part1.functional_data_structures.MyList._

import scala.collection.mutable.ArrayBuffer

class List$Test extends FunSuite {

  val empty: MyList[Double] = Nil
  val justHead: MyList[Int] = Cons(1, Nil)
  val twoItems: MyList[String] = Cons("a", Cons("b", Nil))

  val threeItems: MyList[String] = Cons("f", twoItems)

  /** exercise 3.2 */
  test("Nil tail is Nil") {
    assertResult(Nil)(myTail(empty))
  }

  test("Single item list tail is Nil") {
    assertResult(Nil)(myTail(justHead))
  }
  test("Double item list tail gives second item") {
    assertResult(Cons("b", Nil))(myTail(twoItems))
  }

  /** exercise 3.3 */
  test("Can't set the head of a Nil") {
    assertResult(Nil)(setHead(1.0, empty))
  }

  test("Set the head of a single item list") {
    assertResult(Cons(1, Nil))(setHead(1, justHead))
  }

  test("Set head of double item list") {
    assertResult(Cons("c", Cons("b", Nil)))(setHead("c", twoItems))
  }

  /** exercise 3.4 */
  test("Dropping items of Nil gives Nil") {
    assertResult(Nil)(drop(678, empty))

  }

  test("Dropping negative or zero items gives same list") {
    assertResult(twoItems)(drop(-45, twoItems))
    assertResult(twoItems)(drop(0, twoItems))
  }

  test("Dropping more or equal to list size items gives Nil") {
    assertResult(Nil)(drop(2, twoItems))
    assertResult(Nil)(drop(45, twoItems))
  }

  test("Dropping less than list size items gives tail") {
    assertResult(twoItems)(drop(1, threeItems))
  }

  /** exercise 3.5 */
  test("Dropping items of Nil is Nil no matter the predicate") {
    assertResult(Nil)(dropWhile(empty, (x: Double) => true))
    assertResult(Nil)(dropWhile(empty, (x: Double) => false))

  }

  test("Dropping items while true is Nil ") {
    assertResult(Nil)(dropWhile(threeItems, (x: String) => true))
  }

  test("Dropping items while false gives initial list") {
    assertResult(threeItems)(dropWhile(threeItems, (x: String) => false))
  }

  test("Lets drop first element") {
    assertResult(twoItems)(dropWhile(threeItems, (x: String) => x == "f"))
  }

  test("Lets drop first and second element") {
    assertResult(Cons("b", Nil))(dropWhile(threeItems, (x: String) => x == "f" || x == "a"))
  }

  test("Lets drop all three elements") {
    assertResult(Nil)(dropWhile(threeItems, (x: String) => x == "f" || x == "a" || x == "b"))
  }

  /** exercise 3.6 */
  test("init from Nil is nil") {
    assertResult(Nil)(init(Nil))
  }

  test("init from single item list is Nil") {
    assertResult(Nil)(init(justHead))
  }

  test("init from double item list is list with only head") {
    assertResult(Cons("a", Nil))(init(twoItems))
  }

  test("init from triple item list is list with first two items") {
    assertResult(Cons("f", Cons("a", Nil)))(init(threeItems))
  }

  /** product2 testing */
  test("product2 with foldRight ") {
    assertResult(8)(product2(MyList(2, 2, 2)))
  }

  test("product2 with foldRight should return false") {
    assertResult(false)(product2(MyList(2, 2, 2)) == 9)
  }

  test("product2 what happens if List contains 0 in items") {
    assertResult(0)(product2(MyList(0, 2, 2, 0)))
  }

  /** exercise 3.8 */
  test("foldright is Cons and Nil") {
    assertResult(Cons(1, Cons(2, Cons(3, Nil))))(foldRight(MyList(1, 2, 3), Nil: MyList[Int])(Cons(_, _)))
  }

  /** exercise 3.9 */
  test("Nil has zero items") {
    assert(length(Nil) == 0)
  }

  test("single item list has size one") {
    assert(length(justHead) == 1)
  }

  test("double item list has size two") {
    assert(length(twoItems) == 2)
  }

  test("triple item list has size three") {
    assert(length(threeItems) == 3)
  }

  test("length2 | Nil has size zero") {
    assert(length2(Nil) == 0)
  }

  test("length2 | single item list has size one") {
    assert(length2(justHead) == 1)
  }

  test("length2 | double item list has size two") {
    assert(length2(twoItems) == 2)
  }

  test("length2 | triple item list has size three") {
    assert(length2(threeItems) == 3)
  }

  /** exercise 3.10 */
  test("foldleft addition") {
    assertResult(6)(foldLeft(MyList(1, 2, 3), 0.0)(_ + _))
  }

  /** exercise 3.11 */
  test("foldleft multiplication") {
    assertResult(24)(foldLeft(MyList(1, 2, 3, 4), 1.0)(_ * _))
  }

  /** exercise 3.11 */
  test("foldleft length") {
    assertResult(4)(foldLeft(MyList("a", "b", "c", "d"), 0)((x: Int, y) => 1 + x))
  }

  /** exercise 3.12 */
  test("reverse") {
    assertResult(Nil)(reverse(Nil))
    assertResult(MyList("b", "a", "f"))(reverse(threeItems))
  }

  /** exercise 3.16 */
  test("increment") {
    assertResult(Nil)(increment(Nil, 1))
    assertResult(MyList(2, 3, 4))(increment(MyList(1, 2, 3), 1))
  }

  /** exercise 3.17 */
  test("double2string") {
    assertResult(Nil)(double2string(Nil))
    assertResult(MyList("1.0", "2.0", "3.0"))(double2string(MyList(1, 2, 3)))
  }

  /** exercise 3.18 */
  test("map") {
    assertResult(Nil)(map(Nil)(x => x))
    assertResult(MyList("1.0", "2.0", "3.0"))(map(MyList(1.0, 2.0, 3.0))(x => x.toString))
  }

  /** exercise 3.19 */
  test("filter") {
    assertResult(Nil)(filter(Nil)(x => x))
    assertResult(MyList(2))(filter(MyList(1, 2, 3))(x => x % 2 == 0))
  }

  /** exercise 3.20 */
  test("flatMap") {
    assertResult(Nil)(flatMap(Nil)(x => MyList(x)))
    assertResult(MyList(1,1,2,2,3,3))(flatMap(MyList(1,2,3))(i => MyList(i,i)))

    assertResult(MyList(1,5,2,10,3,15))(flatMap(MyList(1,2,3))(i => MyList(i,i*5)))

    assertResult(MyList(1,2,3,4,5,6,7,8,9))(flatMap(MyList(MyList(1,2,3), MyList(4,5,6), MyList(7,8,9)))(i => i))

  }

  /** exercise 3.21 */
  test("filter2") {
    assertResult(Nil)(filter2(Nil)(x => x))
    assertResult(MyList(2))(filter2(MyList(1, 2, 3))(x => x % 2 == 0))
  }

  /** exercise 3.22 */
  test("zipWithAddInt") {
    assertResult(Nil)(zipWithAddInt(Nil, Nil))
    assertResult(MyList(1,2,3))(zipWithAddInt(MyList(1,2,3), Nil))
    assertResult(MyList(1,2,3))(zipWithAddInt(Nil, MyList(1,2,3)))
    assertResult(MyList(2,4,6))(zipWithAddInt(MyList(1,2,3), MyList(1,2,3)))
    assertResult(MyList(2,4,6,4))(zipWithAddInt(MyList(1,2,3), MyList(1,2,3,4)))

  }

  /** exercise 3.22 */
  test("zipWithAddInt2") {
    assertResult(Nil)(zipWithAddInt2(Nil, Nil))
    assertResult(Nil)(zipWithAddInt2(MyList(1,2,3), Nil))
    assertResult(Nil)(zipWithAddInt2(Nil, MyList(1,2,3)))
    assertResult(MyList(2,4,6))(zipWithAddInt2(MyList(1,2,3), MyList(1,2,3)))

  }

  /** exercise 3.23 */
  test("zipWith") {
    assertResult(Nil)(zipWith(Nil, Nil)((x,y) => x))
    assertResult(Nil)(zipWith(MyList(1,2,3), Nil)((x,y) => x))
    assertResult(Nil)(zipWith(Nil, MyList(1,2,3))((x,y) => x))
    assertResult(MyList(1,2,3))(zipWith(MyList(1,2,3), MyList(1,2,3))((x,y) => x))
    assertResult(MyList(1,4,9))(zipWith(MyList(1,2,3), MyList(1,2,3))((x,y) => x * y))
  }

  /** exercise 3.24 */
  test("hasSubsequence") {
    assertResult(false)(hasSubsequence(Nil, Nil))
    assertResult(false)(hasSubsequence(MyList(0,1,2,3,1,2,2), MyList(1,2,5)))
    assertResult(true)(hasSubsequence(MyList(0,1,2,3,1,2,2), MyList(1,2,3)))
    assertResult(true)(hasSubsequence(MyList("a","b","c","d","e","f","u","c","k"), MyList("f","u")))
    assertResult(false)(hasSubsequence(MyList("a","b","c","d","e","f","u","c","k"), MyList("f","u","k")))

  }

}
