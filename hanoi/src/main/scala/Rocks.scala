
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

    if (state.size < 3
      || state.get(source).isEmpty) {
      println("invalid state")

      this
    } else {

      if (disk <= 0) {
        move(disk, source, buffer, destn)
      }
      else {

        hanoi(disk - 1, source, destn, buffer)
          .move(disk, source, buffer, destn)
          .hanoi(disk - 1, buffer, source, destn)
      }
    }
  }

  def move(disk: Int, source: Peg, buffer: Peg, destn: Peg): Rocks = {
    dump
    println("disk " + disk)

    new Rocks(Map(source -> state(source).tail,
      buffer -> state(buffer),
      destn -> (disk :: state(destn))))
  }
}

object Main extends App {
  Rocks.solve(4).dump
}


