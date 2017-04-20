import forcomp.Anagrams._

val s = List("ena", "dyo", "tria", "ane")
def wordOccurrences(w: Word): Occurrences = {
  w.toLowerCase
    .toCharArray
    .groupBy(x => x)
    .map(y => (y._1, y._2.length))
    .toList
    .sortWith(_._1 < _._1)
}



val koko = wordOccurrences("koko")

val kk = wordOccurrences("kk")

val oo = wordOccurrences("oo")

val str = "koko"


val kapa =  for( i <- 1 to 2 ) yield ('k', i )

val opa =  for( i <- 1 to 2 ) yield ('o', i )

def stretch(ch: Char, i: Int): Occurrences = {
  for(j <- (1 to i).toList) yield (ch, j)
}



def correlate[T](a: List[T], b: List[T]): List[List[T]] = {
  val as = for(i <- a) yield List(i)
  val bs = for(i <- b) yield List(i)
  val comb = for(i <- a; j <- b) yield List(i, j)
  List(List[T]()) ++ as ++ bs ++ comb
}

correlate(kapa.toList, opa.toList)

def selfRelate[T](li: List[T]): List[(T, T)] = li match {
      case Nil => Nil
      case h::t => {
        val r: List[(T, T)] = for(i <- t) yield (h, i)
        r ++ selfRelate(t)
      }
}

val stretched = for(k <- koko) yield stretch(k._1, k._2)

val self = selfRelate(stretched)

val total = for(s <- self) yield correlate(s._1, s._2)

total.flatten









