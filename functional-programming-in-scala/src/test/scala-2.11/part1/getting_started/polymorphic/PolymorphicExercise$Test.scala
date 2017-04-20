package part1.getting_started.polymorphic

import org.scalatest.FunSuite

/**
  * Created by alx on 27/10/2016.
  */
class PolymorphicExercise$Test extends FunSuite {
import PolymorphicExercise._

  test("test if Array IsSorted") {
    assertResult(true)(isSortedEx(Array(1,2,2,3), (a:Int, b:Int) => a <= b))
  }

}
