package example

/**
  * Resolves a penguin puzzle
  */
object PenguinGame {

  val printer = new HexagonGridPrinter()

  def main(args: Array[String]): Unit = {

    // Set up a basic game
    val grid0 = HexagonGrid(5, 4, shiftOddDown = false) :+
      (0, 0, Pawn()) :+
      (3, 1, Pawn()) :+
      (1, 2, Pawn()) :+
      (2, 3, Pawn())

    println("Empty board:")
    println(printer render grid0)

    // Classic shapes
    val shapes = List(Shape1, Shape2, Shape3, Shape4)

    // All possible positions for individual shapes (when pawns are in place)
    val shapesAndPositions = shapes.map(s => (s, getPossiblePositions(grid0, s)))

    // Now try them all together
    val results = shapesAndPositions
      .foldLeft(Seq(grid0))((grids, tuple) => applyShapeToGrids(grids, tuple._1, tuple._2))

    println("Solutions:")
    results foreach (g => println(printer render g))
  }

  private def applyShapeToGrids(grids: Seq[HexagonGrid], shape: Shape, positions: Seq[Position]): Seq[HexagonGrid] = {
    // TODO : use a method that returns an Option[HexagonGrid]
    for (g <- grids; p <- positions if g.canAdd(p._1, p._2, shape.rotate(p._3)))
      yield g :+ (p._1, p._2, shape.rotate(p._3))
  }

  private type Position = (Int, Int, Int) // x, y, rotation

  private def getPossiblePositions(grid: HexagonGrid, shape: Shape): Seq[Position] =
    for (y <- 0 until grid.height;
         x <- 0 until grid.width;
         r <- 0 until 6 if grid.canAdd(x, y, shape.rotate(r)))
      yield (x, y, r)
}
