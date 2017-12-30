
trait Peg
/**three singletons composed of an empty trait*/
case object SOURCE extends Peg

case object BUFFER extends Peg

case object DESTINATION extends Peg

/**companion object*/
object Rocks {
/**type of the algorithm state*/
  type State = Map[Peg, List[Int]]
/**initialization function*/
  def init (n: Int): Rocks = new Rocks(
    Map(SOURCE -> (0 until n).toList,
      BUFFER -> Nil,
      DESTINATION -> Nil))
/**static interface to algorithm*/
  def solve(n: Int): Rocks = {
/**construct a problem of size n and solve it*/
    init(n).hanoi(n - 1, SOURCE, BUFFER, DESTINATION)
  }
}

/**the algorithm base class*/
class Rocks(state : Rocks.State) {

  def dump = {
/**state values are lists of ints so they can be printed*/
    println("Source: " + state(SOURCE))

    println("Buffer: " + state(BUFFER))

    println("Destination: " + state(DESTINATION))

  }

 /**recursive algorithm. cannot be made tail recursive!!*/ 
  def hanoi(disk: Int, source: Peg, buffer: Peg, destn: Peg): Rocks = {

    if (disk <= 0) {
      /** move the smallest disk */
      move(disk, source, buffer, destn)
    } else {
      /** move all smaller disks to buffer */
      hanoi(disk - 1, source, destn, buffer)

        /** move the current disk */
        .move(disk, source, buffer, destn)

        /** retrieve all other disks from buffer */
        .hanoi(disk - 1, buffer, source, destn)
    }
  }
/**functional paradigm: in every move returns a new instance of the base class
* with the updated state 
*/
  def move(disk: Int, source: Peg, buffer: Peg, destn: Peg): Rocks = {
    dump
    println("disk " + disk)

    if (state.size < 3
      || state.get(source).isEmpty) {
      println("invalid state")

      this
    } else {

      new Rocks(Map(source -> state(source).tail,
        buffer -> state(buffer),
        destn -> (disk :: state(destn))))
    }
  }
}
/**singleton app*/
object Main extends App {
  Rocks.solve(16).dump
}



