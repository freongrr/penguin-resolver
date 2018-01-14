package com.github.freongrr.penguinresolver

import com.github.freongrr.penguinresolver.grid.HexaDirections.{Down, DownRight, Up, UpRight}
import com.github.freongrr.penguinresolver.grid._
import com.github.freongrr.penguinresolver.printer.HexagonGridPrinter

import scala.util.Success

/**
  * Resolves a penguin puzzle
  */
class PenguinGameResolver(grid: HexagonGrid, shapes: Seq[Shape] = List(Shape1, Shape2, Shape3, Shape4)) {

  private case class Position(x: Int, y: Int, r: Int)

  private val FullRotation = 6

  def resolve(): Seq[HexagonGrid] = {
    // Find valid positions for each shape with an empty board (only pawns)
    // This reduces greatly the number of combinations we have to try after 
    val shapesAndPositions = shapes.map(s => (s, getPossiblePositions(grid, s)))

    // TODO : filter out symmetric shapes

    // Now apply these positions to the grids sequentially
    shapesAndPositions.foldLeft(Seq(grid))((grids, tuple) => {
      val (shape, positions) = tuple
      println(s"Trying $shape on ${grids.length} grids with ${positions.length} positions...")
      applyShapeToGrids(grids, shape, positions)
    })
  }

  private def getPossiblePositions(grid: HexagonGrid, shape: Shape): Seq[Position] =
    for (y <- 0 until grid.height;
         x <- 0 until grid.width;
         r <- 0 until FullRotation if grid.tryAdd(x, y, shape.rotate(r)).isSuccess)
      yield Position(x, y, r)

  private def applyShapeToGrids(grids: Seq[HexagonGrid], shape: Shape, positions: Seq[Position]): Seq[HexagonGrid] = {
    val tried = for (g <- grids; p <- positions)
      yield g.tryAdd(p.x, p.y, shape.rotate(p.r))
    tried.collect({
      case Success(g) => g
    })
  }
}

// Common shapes

object Shape1 extends Shape(Seq(UpRight, Down, Down))

object Shape2 extends Shape(Seq(Up, UpRight, DownRight))

object Shape3 extends Shape(Seq(DownRight, UpRight, DownRight))

object Shape4 extends Shape(Seq(UpRight, UpRight, DownRight))

object PenguinGameResolver {

  def main(args: Array[String]): Unit = {
    val printer = new HexagonGridPrinter()

    val grid = HexagonGrid(5, 4, shiftOddDown = false) :+
      (0, 0, Pawn()) :+
      (3, 1, Pawn()) :+
      (1, 2, Pawn()) :+
      (2, 3, Pawn())

    println("Empty board:")
    println(printer render grid)

    val resolver = new PenguinGameResolver(grid)

    println("Resolving...")
    val start = System.currentTimeMillis()
    val solutionGrids = resolver.resolve()

    println(s"Found ${solutionGrids.length} solutions in ${System.currentTimeMillis() - start} ms:")
    solutionGrids foreach (g => println(printer render g))
  }
}
