package example

import example.HexaDirections._

import scala.collection.mutable

/**
  * Resolves a penguin puzzle
  */
object PenguinGame {

  val printer = new HexagonGridPrinter()

  def main(args: Array[String]): Unit = {

    // Set up a basic game
    val grid0 = new HexagonGrid(5, 4, shiftOddDown = false)
    grid0 += (0, 0, Pawn())
    grid0 += (3, 1, Pawn())
    grid0 += (1, 2, Pawn())
    grid0 += (2, 3, Pawn())

    println("Empty board:")
    println(printer render grid0)

    // Classic shapes
    val shape1 = Shape(Seq(UpRight, Down, Down))
    val shape2 = Shape(Seq(Up, UpRight, DownRight))
    val shape3 = Shape(Seq(DownRight, UpRight, DownRight))
    val shape4 = Shape(Seq(UpRight, UpRight, DownRight))

    // All possible positions for individual shapes (when pawns are in place)
    val positions1 = getPossiblePositions(grid0, shape1)
    val positions2 = getPossiblePositions(grid0, shape2)
    val positions3 = getPossiblePositions(grid0, shape3)
    val positions4 = getPossiblePositions(grid0, shape4)

    // Now try them all together
    // TODO : iterate on shapes instead of hard-coding the calls
    tryPositions(grid0, shape1, positions1, (p1, grid1) => {
      tryPositions(grid1, shape2, positions2, (p2, grid2) => {
        tryPositions(grid2, shape3, positions3, (p3, grid3) => {
          tryPositions(grid3, shape4, positions4, (p4, grid4) => {
            println(s"$p1 - $p2 - $p3 - $p4")
            println(printer render grid4)
          })
        })
      })
    })
  }

  private type Position = (Int, Int, Int) // x, y, rotation

  private def getPossiblePositions(grid: HexagonGrid, shape: Shape): Seq[Position] = {
    val results = new mutable.MutableList[Position]
    for (y <- 0 until grid.height) {
      for (x <- 0 until grid.width) {
        for (r <- 0 until 6) {
          val rotated = shape.rotate(r)
          // TODO - skip symmetric rotations
          if ((grid :+ (x, y, rotated)).isSuccess) {
            results += ((x, y, r))
          }
        }
      }
    }
    results
  }

  private def tryPositions(grid: HexagonGrid, shape: Shape, positions: Seq[Position], function: (Position, HexagonGrid) => Unit): Unit = {
    positions.foreach(p => {
      val triedGrid = grid :+ (p._1, p._2, shape.rotate(p._3))
      if (triedGrid.isSuccess) {
        function(p, triedGrid.get)
      }
    })
  }
}
