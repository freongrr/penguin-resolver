package example

import example.HexaDirections._

import scala.collection.mutable.ArrayBuffer

class HexagonGridPrinter {

  private def CELL_COMPARATOR = Ordering.by[Cell, Int](_.content match {
    case Empty => 1
    case Pawn() => 2
    case ShapeSegment(_) => 3
  })

  /**
    * Renders a HexagonGame to a StringBuilder.
    *
    * @param grid the grid to render
    * @return a string representation of the game
    */
  def render(grid: HexagonGrid): String = {
    val buffer = new RenderBuffer(grid)

    // Sort cells by depth
    val sortedCells = grid.cells.sorted(CELL_COMPARATOR)

    sortedCells foreach (cell => {
      val cellRenderBuffer = buffer.forCell(cell)
      cell.content match {
        case Empty => renderEmptyCell(cellRenderBuffer)
        case ShapeSegment(openSides) => renderShape(cellRenderBuffer, openSides)
        case Pawn() => renderPawn(cellRenderBuffer)
        case _ => ()
      }
    })

    buffer.string
  }

  private def renderEmptyCell(renderBuffer: CellRenderBuffer): Unit = {
    renderBuffer.setString(0, 2, "__", AnsiColors.ANSI_BLUE)
    renderBuffer.setString(1, 1, "/~~\\", AnsiColors.ANSI_BLUE)
    renderBuffer.setString(2, 1, "\\__/", AnsiColors.ANSI_BLUE)
  }

  private def renderShape(renderBuffer: CellRenderBuffer, openSides: Seq[HexaDirection]): Unit = {
    renderBuffer.setString(1, 2, "  ", AnsiColors.ANSI_WHITE)
    if (!openSides.contains(UpLeft)) renderBuffer.setString(1, 1, "/", AnsiColors.ANSI_WHITE)
    if (!openSides.contains(Up)) renderBuffer.setString(0, 2, "__", AnsiColors.ANSI_WHITE)
    if (!openSides.contains(UpRight)) renderBuffer.setString(1, 4, "\\", AnsiColors.ANSI_WHITE)
    if (!openSides.contains(DownLeft)) renderBuffer.setString(2, 1, "\\", AnsiColors.ANSI_WHITE)
    if (!openSides.contains(Down)) renderBuffer.setString(2, 2, "__", AnsiColors.ANSI_WHITE)
    if (!openSides.contains(DownRight)) renderBuffer.setString(2, 4, "/", AnsiColors.ANSI_WHITE)
  }

  private def renderPawn(renderBuffer: CellRenderBuffer): Unit = {
    renderBuffer.setString(0, 2, "__", AnsiColors.ANSI_PURPLE)
    renderBuffer.setString(1, 1, "/", AnsiColors.ANSI_PURPLE)
    renderBuffer.setString(1, 2, "oo", AnsiColors.ANSI_YELLOW)
    renderBuffer.setString(1, 4, "\\", AnsiColors.ANSI_PURPLE)
    renderBuffer.setString(2, 1, "\\__/", AnsiColors.ANSI_PURPLE)
  }
}

// TODO : This is terribly inefficient!
private class RenderBuffer(grid: HexagonGrid, buffer: ArrayBuffer[ArrayBuffer[String]]) {

  def this(grid: HexagonGrid) = this(grid, new ArrayBuffer[ArrayBuffer[String]](grid.height))

  {
    val width = charWidth(grid.width)
    val height = charHeight(grid.width, grid.height)
    initBuffer(width, height)
  }

  private def charWidth(width: Int): Int = {
    width match {
      case 0 => 0
      case _ => 3 + width * 3
    }
  }

  private def charHeight(width: Int, height: Int): Int = {
    width match {
      case 1 =>
        height match {
          case 0 => 0
          case 1 => 3
          // Add 2 rows of text for each row of hexagons
          case _ => charHeight(width, height - 1) + 2
        }
      // Add 1 line of text for hexagons shifted down 
      case _ => charHeight(1, height) + 1
    }
  }

  private def initBuffer(width: Int, height: Int): Unit = {
    for (_ <- 0 until height) {
      val lineBuffer = new ArrayBuffer[String](width + 1)
      buffer += lineBuffer
      for (_ <- 0 until width) {
        lineBuffer += " "
      }
      lineBuffer += s"\n"
    }
  }

  def forCell(cell: Cell): CellRenderBuffer = {
    new CellRenderBuffer(this.buffer, grid, cell)
  }

  def string: String = {
    val builder = new StringBuilder
    buffer foreach (lb => lb foreach builder.++=)
    builder.result
  }
}

private class CellRenderBuffer(val buffer: ArrayBuffer[ArrayBuffer[String]], val grid: HexagonGrid, val cell: Cell) {

  def setString(lineOffset: Int, colOffset: Int, str: String, color: String): Unit = {
    val line = adjustLine(cell.x, cell.y) + lineOffset
    val col = (cell.x * 3) + colOffset
    for (i <- 0 until str.length) {
      val c = str.charAt(i)
      val lineBuffer = buffer(line)
      lineBuffer(col + i) = color + c + AnsiColors.ANSI_RESET
    }
  }

  private def adjustLine(x: Int, y: Int) = {
    val evenColumnOffset = if (grid.shiftOddDown) 0 else 1
    val oddColumnOffset = if (grid.shiftOddDown) 1 else 0
    2 * y + (if (x % 2 == 0) evenColumnOffset else oddColumnOffset)
  }
}
