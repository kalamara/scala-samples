
trait Peg
case object SOURCE extends Peg

case object BUFFER extends Peg

case object DESTINATION extends Peg

object Rocks {
  type State = Map[Peg, List[Int]]

  def init (n: Int): Rocks = new Rocks(
    Map(SOURCE -> (0 until n).toList,
      BUFFER -> Nil,
      DESTINATION -> Nil))

  def solve(n: Int): Rocks = {
    init(n).hanoi(n - 1, SOURCE, BUFFER, DESTINATION)

  }
}

class Rocks(state : Rocks.State) {

  def dump = {

    println("Source: " + state(SOURCE))

    println("Buffer: " + state(BUFFER))

    println("Destination: " + state(DESTINATION))

  }

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

object Main extends App {
  Rocks.solve(16).dump
}



