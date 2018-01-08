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
    update(x, y, pawn)
  }

  // TODO : replace with immutable method - e.g. def ::(...): Try[HexagonGrid]
  def +=(x: Int, y: Int, shape: Shape): Try[Unit] = {
    if (testShape(x, y, shape)) {
      segmentShape(x, y, shape).foreach(tuple => update(tuple._1, tuple._2, tuple._3))
      Success()
    } else {
      Failure(new IllegalArgumentException("Can't add shape here"))
    }
  }

  private def testShape(x: Int, y: Int, shape: Shape): Boolean = {
    val segments = segmentShape(x, y, shape)
    segments.forall(tuple => {
      val (xx, yy, _) = tuple
      try {
        val cell = apply(xx, yy)
        cell.content match {
          case Empty => true
          case _ => false
        }
      } catch {
        case _: Exception => false
      }
    })
  }

  /* Shape with 2 direction changes:
   * - DownRight
   * - UpRight
   *   __    __  
   *  /0 \__/2 \ 
   *  \__/1 \__/ 
   *  /  \__/  \
   *  
   * Should give 3 segments:
   * - 0,0: [DownRight]
   * - 1,0: [UpLeft, UpRight]
   * - 2,0: [DownLeft]
   */

  // This method is bad...
  // This would be easier if the CellContent had the position too, but then it would be a Cell
  // TODO : Try returning a Sequence of Cell instead! 
  private def segmentShape(x: Int, y: Int, shape: Shape): Seq[(Int, Int, ShapeSegment)] = {
    var segments = List((x, y, shape.at(0)))
    var pos = (x, y)
    0 until shape.length foreach (i => {
      val d = shape.directions(i)
      pos = updatePosition(pos, d)
      segments = segments :+ (pos._1, pos._2, shape.at(i + 1))
    })
    segments
  }

  def apply(x: Int, y: Int): Cell = {
    if (x < 0 || x >= width) throw new IllegalArgumentException(s"x is outside bounds [0:$width]")
    if (y < 0 || y >= height) throw new IllegalArgumentException(s"y is outside bounds [0:$height]")
    _cells(y * width + x)
  }

  def update(x: Int, y: Int, content: CellContent): Try[Unit] = {
    try {
      val cell = apply(x, y)
      cell.content match {
        case Empty =>
          println(s"Setting content of $x, $y to $content")
          doUpdate(x, y, content)
          Success()
        case _ =>
          println(s"Can't add $content at $x, $y")
          Failure(new IllegalArgumentException("Can't add shape here"))
      }
    } catch {
      case e: Exception =>
        println(s"Can't add $content at $x, $y: " + e)
        Failure(e)
    }
  }

  private def doUpdate(x: Int, y: Int, content: CellContent): Unit = {
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
}
