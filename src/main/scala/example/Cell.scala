package example

sealed trait Cell {

  def x: Int

  def y: Int
}

/**
  * Represents an empty cell
  */
case class EmptyCell(x: Int, y: Int) extends Cell

case class OccupiedCell(x: Int, y: Int, content: CellContent) extends Cell
