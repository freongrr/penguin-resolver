package com.github.freongrr.penguinresolver

import com.github.freongrr.penguinresolver.grid.HexaDirections.{Down, DownRight, Up, UpRight}
import com.github.freongrr.penguinresolver.grid._
import com.github.freongrr.penguinresolver.printer.HexagonGridPrinter

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

    // Get possible positions for each shape with pawns on the board
    val shapesAndPositions = shapes.map(s => (s, getPossiblePositions(grid0, s)))

    // Now try them all together
    val results = shapesAndPositions
      .foldLeft(Seq(grid0))((grids, tuple) => {
        val (shape, positions) = tuple
        applyShapeToGrids(grids, shape, positions)
      })

    println("Solutions:")
    results foreach (g => println(printer render g))
  }


  private type Position = (Int, Int, Int) // x, y, rotation

  private def FullRotation = 6

  private def getPossiblePositions(grid: HexagonGrid, shape: Shape): Seq[Position] =
    for (y <- 0 until grid.height;
         x <- 0 until grid.width;
         r <- 0 until FullRotation if grid.canAdd(x, y, shape.rotate(r)))
      yield (x, y, r)

  private def applyShapeToGrids(grids: Seq[HexagonGrid], shape: Shape, positions: Seq[Position]): Seq[HexagonGrid] = {
    // TODO : use a method that returns an Option[HexagonGrid] and match it instead of calling canAdd
    for (g <- grids; p <- positions if g.canAdd(p._1, p._2, shape.rotate(p._3)))
      yield g :+ (p._1, p._2, shape.rotate(p._3))
  }
}

// Common shapes

object Shape1 extends Shape(Seq(UpRight, Down, Down))

object Shape2 extends Shape(Seq(Up, UpRight, DownRight))

object Shape3 extends Shape(Seq(DownRight, UpRight, DownRight))

object Shape4 extends Shape(Seq(UpRight, UpRight, DownRight))
