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

  // TODO : replace with immutable method - e.g. def ::(...): Try[HexagonGrid]
  def +=(x: Int, y: Int, pawn: Pawn): Try[Unit] = {
    if (isCellEmpty(x, y)) {
      doUpdate(x, y, pawn)
      Success()
    } else {
      log(s"Can't add $pawn at $x, $y")
      Failure(new IllegalArgumentException("Can't add shape here"))
    }
  }

  // TODO : replace with immutable method - e.g. def ::(...): Try[HexagonGrid]
  def +=(x: Int, y: Int, shape: Shape): Try[Unit] = {
    val shapeCells = getShapeCell(x, y, shape)
    val canAddShape = shapeCells.forall(c => isCellEmpty(c.x, c.y))
    if (canAddShape) {
      shapeCells.foreach(tuple => doUpdate(tuple.x, tuple.y, tuple.content))
      Success()
    } else {
      log(s"Can't add $shape at $x, $y")
      Failure(new IllegalArgumentException("Can't add shape here"))
    }
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
    println("[LOG] " + str)
  }
}
