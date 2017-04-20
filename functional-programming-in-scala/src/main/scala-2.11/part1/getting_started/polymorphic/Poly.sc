import part1.error_handling.TheSome

/*
import part1.functional_data_structures.{Cons, MyList}
import MyList._


def filter(xs: List[Int], p: Int => Boolean): List[Int] =
  if (xs.isEmpty) xs
  else if (p(xs.head)) xs.head :: filter(xs.tail, p)
  else filter(xs.tail, p)

def modN(n: Int)(x: Int) = ( (x % n) == 0)

val nums = List(1,2,3,4,5,6,7,8,9,10)

filter(nums, modN(3))

def filt(devider: Int) (x: Int) = x % devider == 0 || x % 3 == 0
filt(2)(4)
nums.filter(modN(3))
nums.filter(filt(2))
//nums.filter(x => x % 2 == 0)


val ll = MyList(1,2,3)
//foldRight(MyList(1,2,3), MyList[Int])(Cons(_,_))

def uncurry[A,B,C](f: A => B => C): (A, B) => C =
  (a, b) => f(a)(b)

def compose[A,B,C](f: B => C, g: A => B): A => C =
  (a) => f(g(a))

def f = (i: Int) => i / 2
def g = (i: Int) => i * 10

compose(f,g)(2)

append(MyList(1,2), MyList(3,4))
appendFoldRight(MyList(1,2), MyList(3,4,5,6,7))
appendFoldLeft(MyList(1,2,3,4,5), MyList(6,7,8,9))
*/
val li = List(1,2).map(a=>a)
(1 to 3).scan(10)(_-_)
(1 to 3).scanLeft(10)(_-_)

//import part1.error_handling.{TheSome, TheOption}
val op = TheSome("koko")
op.map(x => x == "koko")
