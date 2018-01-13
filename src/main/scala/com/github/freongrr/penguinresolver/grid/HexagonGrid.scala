package com.github.freongrr.penguinresolver.grid

import com.github.freongrr.penguinresolver.grid.HexaDirections._

import scala.util.Try

class HexagonGrid(val width: Int, val height: Int, val shiftOddDown: Boolean, val cells: Seq[Cell]) {

  /**
    * Returns the Cell at the given position. Throws IllegalArgumentException if the requested position is invalid.
    *
    * @param x the column (>= 0 and < width)
    * @param y the row (>= 0 and < height)
    * @return the Cell
    */
  def apply(x: Int, y: Int): Cell = {
    if (x < 0 || x >= width) throw new IllegalArgumentException(s"x: $x is outside bounds [0:$width]")
    if (y < 0 || y >= height) throw new IllegalArgumentException(s"y: $y is outside bounds [0:$height]")
    cells(y * width + x)
  }

  /**
    * Returns a copy of this grid with an additional Pawn, if it can be added.  
    *
    * @param x    the column to add the pawn to (>= 0 and < width)
    * @param y    the row to add the pawn to (>= 0 and < height)
    * @param pawn the pawn to add
    * @return the updated grid
    */
  def :+(x: Int, y: Int, pawn: Pawn): HexagonGrid =
    this.withContent(x, y, pawn: CellContent)

  /**
    * Same as :+ but returns a Try
    *
    * @param x    the column to add the pawn to (>= 0 and < width)
    * @param y    the row to add the pawn to (>= 0 and < height)
    * @param pawn the pawn to add
    * @return the updated grid wrapped in a Success or a Failure
    */
  def tryAdd(x: Int, y: Int, pawn: Pawn): Try[HexagonGrid] = Try(this :+ (x, y, pawn))

  private def withContent(x: Int, y: Int, content: CellContent): HexagonGrid =
    this (x, y) match {
      case EmptyCell(_, _) =>
        val newCells = cells.updated(y * width + x, OccupiedCell(x, y, content))
        HexagonGrid(width, height, shiftOddDown, newCells)
      case _ =>
        throw new IllegalArgumentException(s"Can't add content at $x, $y")
    }

  /**
    * Returns a copy of this grid with an additional Pawn, if it can be added.  
    *
    * @param x     the column to place the start of the shape to (>= 0 and < width)
    * @param y     the row to place start of the shape (>= 0 and < height)
    * @param shape the shape to add
    * @return the updated grid
    */
  def :+(x: Int, y: Int, shape: Shape): HexagonGrid =
    getShapeSegmentCells(x, y, shape).foldLeft(this)((g, c) => g.withContent(c.x, c.y, c.content))

  /**
    * Same as :+ but returns a Try
    *
    * @param x     the column to place the start of the shape to (>= 0 and < width)
    * @param y     the row to place start of the shape (>= 0 and < height)
    * @param shape the shape to add
    * @return the updated grid wrapped in a Success or a Failure
    */
  def tryAdd(x: Int, y: Int, shape: Shape): Try[HexagonGrid] = Try(this :+ (x, y, shape))

  private def getShapeSegmentCells(x: Int, y: Int, shape: Shape): Seq[OccupiedCell] = {
    val firstCell = OccupiedCell(x, y, shape(0))
    shape.directions.foldLeft(Seq(firstCell))((cells, direction) => {
      val (newX, newY) = updatePosition(cells.last.x, cells.last.y, direction)
      cells :+ OccupiedCell(newX, newY, shape(cells.length))
    })
  }

  private def updatePosition(x: Int, y: Int, direction: HexaDirection): (Int, Int) = {
    // whether we change row when going left/right depends on the column and whether we shift up or down
    // e.g. when shifting odd columns down, we use UpRight and then DownRight while staying on row 1
    //     __    __    __  
    //    /  \__/2 \__/  \ 
    //    \__/1 \__/3 \__/ 
    //    /  \__/  \__/  \ 
    //    \__/  \__/  \__/
    (
      direction match {
        case UpRight | DownRight => x + 1
        case UpLeft | DownLeft => x - 1
        case _ => x
      },
      direction match {
        case Up => y - 1
        case Down => y + 1
        case UpLeft | UpRight =>
          if (x % 2 == 0 == this.shiftOddDown) {
            y - 1
          } else {
            y
          }
        case DownLeft | DownRight =>
          if (x % 2 == 0 != this.shiftOddDown) {
            y + 1
          } else {
            y
          }
        case _ => y
      }
    )
  }

  // TODO
  private def log(str: String): Unit = {
    println("[LOG] " + str)
  }
}

object HexagonGrid {

  def apply(width: Int, height: Int, shiftOddDown: Boolean = true): HexagonGrid = {
    val cells = for (y <- 0 until height; x <- 0 until width) yield EmptyCell(x, y)
    HexagonGrid(width, height, shiftOddDown, cells)
  }

  def apply(width: Int, height: Int, shiftOddDown: Boolean, cells: Seq[Cell]): HexagonGrid = {
    new HexagonGrid(width, height, shiftOddDown, cells)
  }
}
