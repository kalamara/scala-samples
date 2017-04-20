package part1.getting_started.polymorphic

import org.scalatest.FunSuite

/**
  * Created by alx on 27/10/2016.
  */
class PolymorphicExample$Test extends FunSuite {

  import PolymorphicExample._

  test("testFindFirstPolymorphic with imlicit Array") {
    implicit val myArrA = Array(1, 2, 3)
    assertResult(0)(findFirstPolymorphic((e: Int) => e == 1 ))
  }

  test("testFindFirst1") {
    assertResult(2)(findFirst1(Array("1","2","3"), "3"))
  }

}
