package part1.functional_data_structures

import org.scalatest.FunSuite

/**
  * Created by alx on 29/11/2016.
  */
class MyTree$Test extends FunSuite {

  import MyTree._

  test("testSize") {
    val leaf1 = Leaf(1)
    val leaf2 = Leaf(1)
    val leaf3 = Leaf(1)
    val branch1 = Branch(leaf2, leaf3)
    val tree = Branch(leaf1, branch1)
    val tree3 = Branch(Branch(Branch(tree, leaf1), leaf1), leaf1)

    assertResult(4)(size(tree3))
    //    assertResult(2)(size(Branch(branch1, leaf1)))
  }

  test("depth") {
    val leaf1 = Leaf(21)
    val leaf2 = Leaf(13)
    val leaf3 = Leaf(14)
    val leaf4 = Leaf(11)
    val leaf5 = Leaf(15)
    val branch1 = Branch(leaf1, leaf2)
    val branch2 = Branch(branch1, leaf3)
    val branch3 = Branch(branch2, leaf4)

    val tree = Branch(branch3, leaf5)

    assertResult(4)(depth(tree))
  }


  test("maximum") {
    val leaf1 = Leaf(21)
    val leaf2 = Leaf(13)
    val leaf3 = Leaf(24)
    val leaf4 = Leaf(11)
    val leaf5 = Leaf(15)
    val branch1 = Branch(leaf1, leaf2)
    val branch2 = Branch(branch1, leaf3)
    val branch3 = Branch(branch2, leaf4)

    val tree = Branch(branch3, leaf5)

    assertResult(24)(maximum(tree))
  }

  test("maximumViaFold") {
    val leaf1 = Leaf(21)
    val leaf2 = Leaf(13)
    val leaf3 = Leaf(24)
    val leaf4 = Leaf(11)
    val leaf5 = Leaf(15)
    val branch1 = Branch(leaf1, leaf2)
    val branch2 = Branch(branch1, leaf3)
    val branch3 = Branch(branch2, leaf4)

    val tree = Branch(branch3, leaf5)

    assertResult(24)(maximumViaFold(tree))
  }

  test("maximum2") {
    val leaf1 = Leaf(21)
    val leaf2 = Leaf(13)
    val leaf3 = Leaf(24)
    val leaf4 = Leaf(11)
    val leaf5 = Leaf(15)
    val branch1 = Branch(leaf1, leaf2)
    val branch2 = Branch(branch1, leaf3)
    val branch3 = Branch(branch2, leaf4)

    val tree = Branch(branch3, leaf5)

    assertResult(24)(maximum2(tree))
  }


  test("map") {
    val leaf1 = Leaf(100)
    val leaf2 = Leaf(100)
    val leaf3 = Leaf(100)
    val leaf4 = Leaf(100)
    val leaf5 = Leaf(100)
    val branch1 = Branch(leaf1, leaf2)
    val branch2 = Branch(branch1, leaf3)
    val branch3 = Branch(branch2, leaf4)

    val tree = Branch(branch3, leaf5)

    val leaf11 = Leaf(200)
    val leaf22 = Leaf(200)
    val leaf33 = Leaf(200)
    val leaf44 = Leaf(200)
    val leaf55 = Leaf(200)
    val branch11 = Branch(leaf11, leaf22)
    val branch22 = Branch(branch11, leaf33)
    val branch33 = Branch(branch22, leaf44)

    val expectedtree = Branch(branch33, leaf55)

    assertResult(expectedtree)(map(tree, (x: Int) => x * 2))
  }

  test("mapViaFold") {
    val leaf1 = Leaf(100)
    val leaf2 = Leaf(100)
    val leaf3 = Leaf(100)
    val leaf4 = Leaf(100)
    val leaf5 = Leaf(100)
    val branch1 = Branch(leaf1, leaf2)
    val branch2 = Branch(branch1, leaf3)
    val branch3 = Branch(branch2, leaf4)

    val tree = Branch(branch3, leaf5)

    val leaf11 = Leaf(200)
    val leaf22 = Leaf(200)
    val leaf33 = Leaf(200)
    val leaf44 = Leaf(200)
    val leaf55 = Leaf(200)
    val branch11 = Branch(leaf11, leaf22)
    val branch22 = Branch(branch11, leaf33)
    val branch33 = Branch(branch22, leaf44)

    val expectedtree = Branch(branch33, leaf55)

    expectedtree
    assertResult(expectedtree)(mapViaFold(tree)((x: Int) => x * 2))
  }


}
