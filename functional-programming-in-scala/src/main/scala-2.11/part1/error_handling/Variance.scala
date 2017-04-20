package part1.error_handling

import scala.annotation.tailrec

object Variance {

  def compute(x: Seq[Double], i: Int, mean: Double, m2: Double): Option[Double] = {

    /*i < 2 match {
      case true => None
      case false => m2 / (i - 1)
    }*/
    None
  }

  def meanOnline(data: Seq[Double]): Double = {
    data match {
      case Nil => 0.0
      case _ =>
        data.head / (data.size) + (data.size - 1 / (data.size)) * meanOnline(data.tail)
    }
  }



  def mean(data: Seq[Double]): Option[Double] = {
    if(data.isEmpty) None
    else Some(data.sum / data.size)
  }

  def variance(data: Seq[Double]): Option[Double] = {
    mean(data).flatMap(m => mean(data.map(x => math.pow(x - m, 2))))
  }

  def varianceOnline(data: Seq[Double]): Option[Double] = {
    None
  }

  }
