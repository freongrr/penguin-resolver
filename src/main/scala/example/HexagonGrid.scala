package example

import scala.util.{Failure, Success, Try}

case class Cell(x: Int, y: Int, content: Any = null)

// TODO
sealed trait Pawn

class DummyPawn extends Pawn {
  
} 

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
    val cell = apply(x, y)

    cell.content match {
      case null => {
        println(s"Adding pawn $pawn")
        update(x, y, pawn)
        Success()
      }
      case _ => {
        Failure(new IllegalArgumentException("Can't add shape here"))
      }
    }
  }

  def apply(x: Int, y: Int): Cell = {
    _cells(y * width + x)
  }

  def update(x: Int, y: Int, content: Any): Unit = {
    _cells.update(y * width + x, Cell(x, y, content))
  }
}
