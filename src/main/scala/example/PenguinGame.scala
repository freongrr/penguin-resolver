package example

import example.HexaDirections._

object PenguinGame {

  def main(args: Array[String]): Unit = {

    val grid = new HexagonGrid(5, 4, shiftOddDown = false)

    grid += (0, 0, Pawn())
    grid += (3, 1, Pawn())
    grid += (1, 2, Pawn())
    grid += (2, 3, Pawn())

    grid += (0, 2, Shape(Seq(Up, UpRight, DownRight)))
    grid += (3, 2, Shape(Seq(UpRight, Down, Down)))

    val printer = new HexagonGridPrinter()
    println(printer.render(grid))
  }
}
