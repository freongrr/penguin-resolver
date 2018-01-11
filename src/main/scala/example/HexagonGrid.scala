package example

import example.HexaDirections._

import scala.util.{Failure, Success, Try}

class HexagonGrid(val width: Int, val height: Int, val shiftOddDown: Boolean = true) {

  private val _cells = new scala.collection.mutable.ArrayBuffer[Cell](width * height)

  {
    for (y <- 0 until height) {
      for (x <- 0 until width) {
        _cells += Cell(x, y)
      }
    }
  }

  def cells: Seq[Cell] = _cells

  /**
    * Mutates the current grid by adding a Pawn.
    *
    * @param x    the column to add the pawn to (>= 0 and < width)
    * @param y    the row to add the pawn to (>= 0 and < height)
    * @param pawn the pawn to add
    * @throws IllegalArgumentException if the Pawn can't be added
    */
  def +=(x: Int, y: Int, pawn: Pawn): Unit = {
    if (isCellEmpty(x, y)) {
      doUpdate(x, y, pawn)
    } else {
      log(s"Can't add $pawn at $x, $y")
      throw new IllegalArgumentException("Can't add shape here")
    }
  }

  /**
    * Returns a copy of this grid with an additional Pawn, if it can bw added. If not returns {Failure}.  
    *
    * @param x    the column to add the pawn to (>= 0 and < width)
    * @param y    the row to add the pawn to (>= 0 and < height)
    * @param pawn the pawn to add
    * @return a Try wrapping the updated grid
    */
  def :+(x: Int, y: Int, pawn: Pawn): Try[HexagonGrid] = {
    val gridCopy = this.copy()
    Try.apply(() => {
      gridCopy += (x, y, pawn)
    }).transform(_ => Success(gridCopy), Failure(_))
  }

  /**
    * Mutates the current grid by adding a Shape.
    *
    * @param x     the column to place the start of the shape to (>= 0 and < width)
    * @param y     the row to place start of the shape (>= 0 and < height)
    * @param shape the shape to add
    * @throws IllegalArgumentException if the Shape can't be added
    */
  def +=(x: Int, y: Int, shape: Shape): Unit = {
    val shapeCells = getShapeCell(x, y, shape)
    val canAddShape = shapeCells.forall(c => isCellEmpty(c.x, c.y))
    if (canAddShape) {
      shapeCells.foreach(c => doUpdate(c.x, c.y, c.content))
    } else {
      log(s"Can't add $shape at $x, $y")
      throw new IllegalArgumentException("Can't add shape here")
    }
  }

  /**
    * Returns a copy of this grid with an additional Pawn, if it can bw added. If not returns {Failure}.  
    *
    * @param x     the column to place the start of the shape to (>= 0 and < width)
    * @param y     the row to place start of the shape (>= 0 and < height)
    * @param shape the shape to add
    * @return a Try wrapping the updated grid
    */
  def :+(x: Int, y: Int, shape: Shape): Try[HexagonGrid] = {
    val gridCopy = this.copy()
    Try.apply(() => {
      gridCopy += (x, y, shape)
    }).transform(_ => Success(gridCopy), Failure(_))
  }

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

  def apply(x: Int, y: Int): Cell = {
    if (x < 0 || x >= width) throw new IllegalArgumentException(s"x is outside bounds [0:$width]")
    if (y < 0 || y >= height) throw new IllegalArgumentException(s"y is outside bounds [0:$height]")
    _cells(y * width + x)
  }

  private def isCellEmpty(x: Int, y: Int) = {
    try {
      val cell = apply(x, y)
      cell.content match {
        case Empty => true
        case _ => false
      }
    } catch {
      case _: Exception => false
    }
  }

  private def copy(): HexagonGrid = {
    val copy = new HexagonGrid(this.width, this.height, this.shiftOddDown)
    copy._cells.clear()
    copy._cells ++= this._cells
    copy
  }

  private def doUpdate(x: Int, y: Int, content: CellContent): Unit = {
    log(s"Setting content of $x, $y to $content")
    _cells.update(y * width + x, Cell(x, y, content))
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
    // println("[LOG] " + str)
  }
}
