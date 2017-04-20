package part1.error_handling

import org.scalatest.FunSuite

/**
  * Created by alx on 2/12/2016.
  */
class InsuranceCalcTest extends FunSuite {

  test("insuranceRateQuote - test if insurance calculator replies correct value") {
    val age = 39
    val numberOfSpeedingTickets = 2

    assertResult(39 * 2 / 10)(new InsuranceCalc().insuranceRateQuote(age, numberOfSpeedingTickets))
  }

  test("parseInsuranceRateQuote with Strign inputs") {
    val age = "39"
    val numberOfSpeedingTickets = "2"

    assertResult(TheSome(39 * 2 / 10))(new InsuranceCalc().parseInsuranceRateQuote(age, numberOfSpeedingTickets))
  }
}
