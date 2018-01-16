package com.github.freongrr.penguinresolver.grid

// TODO : use row and column to avoid confusion with x/y position in pixels?
sealed trait Cell {

  def x: Int

  def y: Int
}

/**
  * Represents an empty cell
  */
case class EmptyCell(x: Int, y: Int) extends Cell

case class OccupiedCell(x: Int, y: Int, content: CellContent) extends Cell
