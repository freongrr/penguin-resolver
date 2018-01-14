package com.github.freongrr.penguinresolver.printer

import com.github.freongrr.penguinresolver.grid.HexaDirections._
import com.github.freongrr.penguinresolver.grid._

/**
  * Renders a HexagonGrid to a String.
  *
  * @param bufferBuilder a constructor for a StringGridBuffer
  */
class HexagonGridPrinter(bufferBuilder: (Int, Int) => StringGridBuffer = StringGridBuffer.color) {

  private def CELL_COMPARATOR = Ordering.by[Cell, Int] {
    case EmptyCell(_, _) => 1
    case OccupiedCell(_, _, content) => content match {
      case Pawn() => 2
      case ShapeSegment(_) => 3
    }
  }

  /**
    * Renders a HexagonGame to a String.
    *
    * @param grid the grid to render
    * @return a string representation of the game
    */
  def render(grid: HexagonGrid): String = {
    val gridRenderBuffer = createBuffer(grid)

    // Render cells sorted by depth
    grid.cells sorted CELL_COMPARATOR foreach (renderCell(gridRenderBuffer, _))

    gridRenderBuffer.asString()
  }

  private def renderCell(gridRenderBuffer: GridRenderBuffer, cell: Cell): Unit = {
    val cellRenderBuffer = gridRenderBuffer.forCell(cell)
    cell match {
      case EmptyCell(_, _) => renderEmptyCell(cellRenderBuffer)
      case OccupiedCell(_, _, content) => content match {
        case ShapeSegment(openSides) => renderShapeSegment(cellRenderBuffer, openSides)
        case Pawn() => renderPawn(cellRenderBuffer)
      }
    }
  }

  private def renderEmptyCell(buffer: CellRenderBuffer): Unit = {
    buffer.write(0, 2, "__", AnsiColors.ANSI_BLUE)
    buffer.write(1, 1, "/~~\\", AnsiColors.ANSI_BLUE)
    buffer.write(2, 1, "\\__/", AnsiColors.ANSI_BLUE)
  }

  private def renderShapeSegment(buffer: CellRenderBuffer, openSides: Seq[HexaDirection]): Unit = {
    buffer.write(1, 2, "  ", AnsiColors.ANSI_WHITE)
    if (!openSides.contains(UpLeft)) buffer.write(1, 1, "/", AnsiColors.ANSI_WHITE)
    if (!openSides.contains(Up)) buffer.write(0, 2, "__", AnsiColors.ANSI_WHITE)
    if (!openSides.contains(UpRight)) buffer.write(1, 4, "\\", AnsiColors.ANSI_WHITE)
    if (!openSides.contains(DownLeft)) buffer.write(2, 1, "\\", AnsiColors.ANSI_WHITE)
    if (!openSides.contains(Down)) buffer.write(2, 2, "__", AnsiColors.ANSI_WHITE)
    if (!openSides.contains(DownRight)) buffer.write(2, 4, "/", AnsiColors.ANSI_WHITE)
  }

  private def renderPawn(buffer: CellRenderBuffer): Unit = {
    buffer.write(0, 2, "__", AnsiColors.ANSI_PURPLE)
    buffer.write(1, 1, "/", AnsiColors.ANSI_PURPLE)
    buffer.write(1, 2, "oo", AnsiColors.ANSI_YELLOW)
    buffer.write(1, 4, "\\", AnsiColors.ANSI_PURPLE)
    buffer.write(2, 1, "\\__/", AnsiColors.ANSI_PURPLE)
  }

  private def createBuffer(grid: HexagonGrid): GridRenderBuffer = {
    val width = widthInCharacters(grid.width)
    val height = heightInCharacters(grid.width, grid.height)
    new GridRenderBuffer(grid, bufferBuilder(width, height))
  }

  private def widthInCharacters(width: Int): Int = {
    width match {
      case 0 => 0
      case _ => 3 + width * 3
    }
  }

  private def heightInCharacters(width: Int, height: Int): Int = {
    width match {
      case 1 =>
        height match {
          case 0 => 0
          case 1 => 3
          // Add 2 rows of text for each row of hexagons
          case _ => heightInCharacters(width, height - 1) + 2
        }
      // Add 1 line of text for hexagons shifted down 
      case _ => heightInCharacters(1, height) + 1
    }
  }
}

private class GridRenderBuffer(grid: HexagonGrid, internalRenderBuffer: StringGridBuffer) {

  def asString(): String = internalRenderBuffer.asString()

  def forCell(cell: Cell): CellRenderBuffer = {
    new CellRenderBuffer(grid, cell, internalRenderBuffer)
  }
}

private class CellRenderBuffer(grid: HexagonGrid, cell: Cell, internalRenderBuffer: StringGridBuffer)
  extends StringGridBuffer {

  override def asString(separator: String): String =
    throw new UnsupportedOperationException("Can't do that")

  override def write(line: Int, col: Int, str: String, option: Any): Unit = {
    val adjustedLine = adjustLine(cell.x, cell.y) + line
    val adjustedCol = (cell.x * 3) + col
    internalRenderBuffer.write(adjustedLine, adjustedCol, str, option)
  }

  private def adjustLine(x: Int, y: Int) = {
    val evenColumnOffset = if (grid.shiftOddDown) 0 else 1
    val oddColumnOffset = if (grid.shiftOddDown) 1 else 0
    2 * y + (if (x % 2 == 0) evenColumnOffset else oddColumnOffset)
  }
}
