package part1.functional_data_structures


sealed trait MyTree[+A]

case class Leaf[A](value: A) extends MyTree[A]

case class Branch[A](left: MyTree[A], right: MyTree[A]) extends MyTree[A]

object MyTree {

  def size[A](t: MyTree[A]): Int = t match {
    case Leaf(_) => 1
    case Branch(l, r) => 1 + size(l) + size(r)
  }


  def depth[A](t: MyTree[A]): Int = t match {
    case Leaf(_) => 0
    case Branch(l, r) => 1 + (depth(l) max depth(r))
  }

  def depth2[A](t: MyTree[A]): Int =
    fold(t)(a => 0)((l, r) => 1 + (l max r))


  def fold[A, B](t: MyTree[A])(f: A => B)(g: (B, B) => B): B = t match {
    case Leaf(l) => f(l)
    case Branch(l, r) => g(fold(l)(f)(g), fold(r)(f)(g))
  }

  def maximumViaFold(t: MyTree[Int]): Int =
    fold(t)(a => a)(_ max _)


  def depth3[A](tree: MyTree[A]): Int = {

    def go(tree: MyTree[A], d: Int): Int = {
      tree match {
        case (a: Leaf[A]) => {
          println("--- Leaf ---")
          d
        }
        case b: Branch[A] => {
          println("--- Branch ---")
          val left = go(b.left, d + 1)
          val right = go(b.right, d + 1)

          if (left <= right) right else left
        }
      }
    }
    go(tree, 0)
  }

  def maximum(t: MyTree[Int]): Int = t match {
    case Leaf(n) => n
    case Branch(l, r) => maximum(l) max maximum(r)
  }

  def maximum2(tree: MyTree[Int]): Int = {
    def go(tree: MyTree[Int], maxValue: Int): Int = {
      tree match {
        case (a: Leaf[Int]) => {
          println("--- Leaf: " + a.value)
          a.value
        }
        case b: Branch[Int] => {
          val left = go(b.left, maxValue)
          val right = go(b.right, maxValue)
          left max right
        }
      }
    }

    go(tree, 0)
  }

  def map[A](tree: MyTree[A], f: A => A): MyTree[A] = {
    tree match {
      case l: Leaf[A] => Leaf(f(l.value))
      case b: Branch[A] => Branch(map(b.left, f), map(b.right, f))
    }
  }

  def mapViaFold[A, B](t: MyTree[A])(f: A => B): MyTree[B] =
    fold(t)(a => Leaf(f(a)): MyTree[B])(Branch(_, _))


}