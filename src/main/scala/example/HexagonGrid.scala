package example

import example.HexaDirections._

class HexagonGrid(val width: Int, val height: Int, val shiftOddDown: Boolean, val cells: Seq[Cell]) {

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
  def :+(x: Int, y: Int, pawn: Pawn): HexagonGrid = {
    this :+ (x, y, pawn: CellContent)
  }

  private def :+(x: Int, y: Int, content: CellContent): HexagonGrid = {
    this (x, y) match {
      case Cell(_, _, Empty) =>
        val newCells = cells.updated(y * width + x, Cell(x, y, content))
        HexagonGrid(width, height, shiftOddDown, newCells)
      case _ =>
        throw new IllegalArgumentException(s"Can't add content at $x, $y")
    }
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
    getShapeCell(x, y, shape).foldLeft(this)((g, c) => g :+ (c.x, c.y, c.content))

  def canAdd(x: Int, y: Int, shape: Shape): Boolean =
    getShapeCell(x, y, shape).forall(c => isCellEmpty(c.x, c.y))

  /**
    * Returns cells that *would* represent a given shape. See Shape#at for more details.
    * HACK - I'm only using Cell to avoid returning Seq[(Int, Int, ShapeSegment)]
    *
    * @param x     the start column of the shape
    * @param y     the start row of the shape
    * @param shape the shape
    * @return a sequence of cells
    */
  private def getShapeCell(x: Int, y: Int, shape: Shape): Seq[Cell] = {
    val firstCell = Cell(x, y, shape.at(0))
    var pos = (x, y)
    List(firstCell) ++ (0 until shape.length).map(i => {
      val d = shape.directions(i)
      pos = updatePosition(pos, d)
      Cell(pos._1, pos._2, shape.at(i + 1))
    })
  }

  private def isCellEmpty(x: Int, y: Int) =
    (x >= 0 && x < width) && (y >= 0 && y < height) && {
      apply(x, y).content match {
        case Empty => true
        case _ => false
      }
    }

  private def updatePosition(pos: (Int, Int), direction: HexaDirection): (Int, Int) = {
    val (x, y) = pos

    // whether we change row when going left/right depends on the column and whether we shift up or down
    // e.g. when shifting odd columns down, we use UpRight and then DownRight while staying on row 1
    //     __    __    __  
    //    /  \__/2 \__/  \ 
    //    \__/1 \__/3 \__/ 
    //    /  \__/  \__/  \ 
    //    \__/  \__/  \__/  

    val newY = direction match {
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

    val newX = direction match {
      case UpRight | DownRight => x + 1
      case UpLeft | DownLeft => x - 1
      case _ => x
    }

    (newX, newY)
  }

  // TODO
  private def log(str: String): Unit = {
    println("[LOG] " + str)
  }
}

object HexagonGrid {

  def apply(width: Int, height: Int, shiftOddDown: Boolean = true): HexagonGrid = {
    val cells = for (y <- 0 until height; x <- 0 until width) yield Cell(x, y)
    HexagonGrid(width, height, shiftOddDown, cells)
  }

  def apply(width: Int, height: Int, shiftOddDown: Boolean, cells: Seq[Cell]): HexagonGrid = {
    new HexagonGrid(width, height, shiftOddDown, cells)
  }
}
