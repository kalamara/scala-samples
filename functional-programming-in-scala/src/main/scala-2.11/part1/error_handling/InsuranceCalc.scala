package part1.error_handling


class InsuranceCalc extends TheSome {

  def insuranceRateQuote(age: Int, numberOfSpeedingTickets: Int): Double =
    age * numberOfSpeedingTickets / 10

  def parseInsuranceRateQuote(age: String, numberOfSpeedingTickets: String): TheOption[Double] = {
    val ageOpt: TheOption[Int] = Try(age.toInt)
    val numbOfSpeedingOpt: TheOption[Int] = Try(numberOfSpeedingTickets.toInt)

    map2(ageOpt, numbOfSpeedingOpt)(insuranceRateQuote)

  }

  def Try[A](a: => A): TheOption[A] =
    try TheSome(a)
    catch {
      case e: Exception => TheNone
    }

}
