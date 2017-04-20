package part1.benefits_of_fp


class Cafe {
  def buyCoffee(cc: CreditCard): (Coffee, Charge) = {
    val cup = new Coffee("espresso", 1.3)

    (cup, Charge(cc, cup.price))
  }

  def buyCoffees(cc: CreditCard, noCoffees: Int): (List[Coffee], Charge) = {
    val purchases: List[(Coffee, Charge)] =  List.fill(noCoffees)(buyCoffee(cc))
    val (coffees, charges) = purchases.unzip

    (coffees, charges.reduce( (c1, c2) => c1.combine(c2)))
  }

}


case class Charge(cc: CreditCard, amount: Double) {
  def combine(other: Charge): Charge = {
    if (cc == other.cc)
      Charge(cc, amount + other.amount)
    else
      throw new Exception("Can't combine these two credit cards")
  }

  def coalence(charges: List[Charge]): List[Charge] =
      charges.groupBy(_.cc).values.map(_.reduce(_ combine _)).toList
}



case class CreditCard(number: Int) {
  def charge(price: Double) = ???
}



case class Coffee(typeOfCoffee: String, price: Double)


case class Payments(transactions: List[Double]) {
  def charge(cc: CreditCard, price: Double) = ???
}