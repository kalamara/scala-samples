package part1.error_handling

import org.scalatest.{BeforeAndAfterEach, FunSuite}

class Variance$Test extends FunSuite with BeforeAndAfterEach {
  val input: Seq[Double] = Seq(0,1,2,3,4,5,6)
  override def beforeEach() {

  }

  override def afterEach() {

  }

  /** exercise 4.2 */
  test("mean should compute mean of a sequence") {

    val exp: Double = 3
    assertResult(Some(exp))(Variance.meanOnline(input))
  }
  test("variance should compute variance of a sequence") {

    val expected: Double = 4 //input.map(x=> math.pow(x - expmean, 2)).sum / 7

    assertResult(Some(expected))(Variance.variance(input))
  }
}
