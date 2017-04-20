package part1.getting_started.hof

import part1.getting_started.HOF.HOFExercise
import part1.getting_started.HOF.HOFExercise._

class ExercisesHOF$Test extends org.scalatest.FunSuite {
      test("Degenerate input gives 1") {
            assertResult(1)(ex2_1_fibonacci(-1))
      }
      test("First Fibonacci is 1") {
            assertResult(1)(ex2_1_fibonacci(0))
      }
      test("Second Fibonacci is 1") {
            assertResult(1)(ex2_1_fibonacci(1))
      }
      test("6th Fibonacci is 13") {
            assertResult(13)(ex2_1_fibonacci(6))
      }



}
