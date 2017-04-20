package funsets

import org.scalatest.FunSuite


import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import scala.math._

/**
 * This class is a test suite for the methods in object FunSets. To run
 * the test suite, you can either:
 *  - run the "test" command in the SBT console
 *  - right-click the file in eclipse and chose "Run As" - "JUnit Test"
 */
@RunWith(classOf[JUnitRunner])
class FunSetSuite extends FunSuite {

  /**
   * Link to the scaladoc - very clear and detailed tutorial of FunSuite
   *
   * http://doc.scalatest.org/1.9.1/index.html#org.scalatest.FunSuite
   *
   * Operators
   *  - test
   *  - ignore
   *  - pending
   */

  /**
   * Tests are written using the "test" operator and the "assert" method.
   */
  // test("string take") {
  //   val message = "hello, world"
  //   assert(message.take(5) == "hello")
  // }

  /**
   * For ScalaTest tests, there exists a special equality operator "===" that
   * can be used inside "assert". If the assertion fails, the two values will
   * be printed in the error message. Otherwise, when using "==", the test
   * error message will only say "assertion failed", without showing the values.
   *
   * Try it out! Change the values so that the assertion fails, and look at the
   * error message.
   */
  // test("adding ints") {
  //   assert(1 + 2 === 3)
  // }


  import FunSets._

  test("contains is implemented") {
    assert(contains(x => true, 100))
  }

  /**
   * When writing tests, one would often like to re-use certain values for multiple
   * tests. For instance, we would like to create an Int-set and have multiple test
   * about it.
   *
   * Instead of copy-pasting the code for creating the set into every test, we can
   * store it in the test class using a val:
   *
   *   val s1 = singletonSet(1)
   *
   * However, what happens if the method "singletonSet" has a bug and crashes? Then
   * the test methods are not even executed, because creating an instance of the
   * test class fails!
   *
   * Therefore, we put the shared values into a separate trait (traits are like
   * abstract classes), and create an instance inside each test method.
   *
   */

  trait TestSets {
    val s1 = singletonSet(1)
    val s2 = singletonSet(2)
    val s3 = singletonSet(3)
  }

  /**
   * This test is currently disabled (by using "ignore") because the method
   * "singletonSet" is not yet implemented and the test would fail.
   *
   * Once you finish your implementation of "singletonSet", exchange the
   * function "ignore" by "test".
   */
  test("singletonSet(1) contains 1") {

    /**
     * We create a new instance of the "TestSets" trait, this gives us access
     * to the values "s1" to "s3".
     */
    new TestSets {
      /**
       * The string argument of "assert" is a message that is printed in case
       * the test fails. This helps identifying which assertion failed.
       */
      assert(contains(s1, 1), "Singleton")
    }
  }

  test("union contains all elements of each set") {
    new TestSets {
      val s = union(s1, s2)
      assert(contains(s, 1), "Union 1")
      assert(contains(s, 2), "Union 2")
      assert(!contains(s, 3), "Union 3")
    }
  }

  test("intersect contains all common elements of all sets") {
    new TestSets {
      val s = intersect(union(s1, s2), union(s1, s3))
      assert(contains(s, 1), "Intersect 1")
      assert(!contains(s, 2), "Intersect 2")
      assert(!contains(s, 3), "Intersect 3")
    }
  }

  test("diff contains all elements of s not in t") {
    new TestSets {
      val s = diff(union(s1, s2), union(s1, s3))
      assert(!contains(s, 1), "diff 1")
      assert(contains(s, 2), "diff 2")
      assert(!contains(s, 3), "diff 3")
    }
  }


  test("filter contains the subset of `s` for which `p` holds") {
    new TestSets {

      val s = filter(union(union(s1, s2), s3), _ % 2 == 0)
      assert(!contains(s, 1), "filter 1")
      assert(contains(s, 2), "filter 2")
      assert(!contains(s, 3), "filter 3")
    }
  }


  test("forall returns whether all bounded integers within `s` satisfy `p`") {
    new TestSets {

      val smallset = union(union(s1, s2), s3)

      val shorts: Set = {x  =>  abs(x) < 65536}

      val positives: Set = {x => x >= 0}

      assert(forall(smallset, _ < 10000), "1,2,3 are < than 10000")
      assert(forall(smallset, _ < 500), "1,2,3 are < than 10000")
      assert(forall(shorts, _ < 100000), "all shorts are < 100000")
      assert(!forall(shorts, _ == 356), "obviously not true for all")
      assert(forall(positives, _ >= 0 ), "obviously true for all")
      assert(!forall(positives, _ <= 0 ), "obviously not true for all")

    }
  }


  test("exists Returns whether there exists a bounded integer within `s`" +
    "  * that satisfies `p`" ) {
    new TestSets {

      val smallset = union(union(s1, s2), s3)

      val shorts: Set = {x  =>  abs(x) < 65536}

      val positives: Set = {x => x >= 0}

      assert(exists(smallset, _ < 10000), "1,2,3 are < than 10000")
      assert(!exists(shorts, _ > 100000), "all shorts are < 100000")
      assert(exists(shorts, _ == 356), "obviously  true for one")
      assert(exists(positives, _ <= 0 ), "obviously not true for all")

    }
  }

  test("map Returns a set transformed by applying `f` to each element of `s`." ) {
    new TestSets {

      val smallset = union(union(s1, s2), s3)

      val s = map(smallset, _ * 2)

      assert(contains(s, 2), "map 1")
      assert(contains(s, 4), "map 2")
      assert(contains(s, 6), "map 3")
      assert(!contains(s, 7), "map 7")

    }
  }



}
