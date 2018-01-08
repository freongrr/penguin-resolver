package example

import example.HexaDirections._

import scala.util.{Failure, Success, Try}

case class Cell(x: Int, y: Int, content: Any = Nil)





class HexagonGrid(val width: Int, val height: Int, val shiftOddDown: Boolean = true) {

  // TODO : how do I init that with empty cells?
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

  def +=(x: Int, y: Int, shape: Shape): Try[Unit] = {
    var pos = (x, y)
    val success = update(pos._1, pos._2, shape).isSuccess &&
      shape.directions.forall(d => {
        pos = updatePosition(pos, d)
        update(pos._1, pos._2, shape).isSuccess
      })

    if (success) {
      Success()
    } else {
      Failure(new IllegalArgumentException("Can't add shape here"))
    }
  }

  def apply(x: Int, y: Int): Cell = {
    if (x < 0 || x >= width) throw new IllegalArgumentException(s"x is outside bounds [0:$width]")
    if (y < 0 || y >= height) throw new IllegalArgumentException(s"y is outside bounds [0:$height]")
    _cells(y * width + x)
  }

  def update(x: Int, y: Int, content: Any): Try[Unit] = {
    try {
      val cell = apply(x, y)
      if (cell.content == Nil) {
        println(s"Setting content of $x, $y to $content")
        doUpdate(x, y, content)
        Success()
      } else {
        println(s"Can't add $content at $x, $y")
        Failure(new IllegalArgumentException("Can't add shape here"))
      }
    } catch {
      case e: Exception =>
        println(s"Can't add $content at $x, $y: " + e)
        Failure(e)
    }
  }

  private def doUpdate(x: Int, y: Int, content: Any): Unit = {
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
