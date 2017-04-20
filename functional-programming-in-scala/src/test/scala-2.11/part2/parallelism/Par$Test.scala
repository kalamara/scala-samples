package part2.parallelism

import java.util
import java.util.concurrent._

import org.scalatest.FunSuite


class Par$Test extends FunSuite {
  val executor = new ForkJoinPool()

  val deadLocker = Executors.newFixedThreadPool(1)

  import Par._
  test("parallel Filter") {
    val testList = (1 to 10).toList

    val expectedList = 2.to(10, 2).toList
    assertResult(expectedList)(parFilter(testList)(_ % 2==0).apply(executor).get())
  }

  test("map") {
    assert(equal(executor)(map(unit(1))(_ + 1),unit(2)))

    def id[A](a: A): A = a
    assert(equal(executor)(map(unit(1))(id),unit(1)))
  }


  test("delay"){
    val a = lazyUnit(42 + 1)
    assert(equal(deadLocker)(delay(a), a)) //if delay is replaced by fork this deadlocks!!
  }

}
