import patmat.Huffman._

val chars: List[Char] = List('a','a', 'b','c')

chars.groupBy(x => x).toList.map(x => (x._1, x._2.size)).sortBy(_._2).map(f => Leaf(f._1, f._2))

decodedSecret

