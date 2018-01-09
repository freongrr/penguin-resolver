package example

import example.HexaDirections._

import scala.collection.mutable.ArrayBuffer

object AnsiColors {

  val ANSI_RESET = "\u001B[0m"
  val ANSI_BLACK = "\u001B[30m"
  val ANSI_RED = "\u001B[31m"
  val ANSI_GREEN = "\u001B[32m"
  val ANSI_YELLOW = "\u001B[33m"
  val ANSI_BLUE = "\u001B[34m"
  val ANSI_PURPLE = "\u001B[35m"
  val ANSI_CYAN = "\u001B[36m"
  val ANSI_WHITE = "\u001B[37m" // TODO 1;37 does not work :(
}

/**
  * TODO : Use different styles for the background and shapes
  */
class HexagonGridPrinter {

  /**
    * Renders a HexagonGame to a StringBuilder.
    *
    * @param grid the grid to render
    * @return a string representation of the game
    */
  def render(grid: HexagonGrid): String = {
    val buffer = new RenderBuffer(grid)

    // TODO : sort the cells instead of doing that?
    renderEmptyCells(grid, buffer)
    renderShapes(grid, buffer)
    renderPawns(grid, buffer)

    buffer.string
  }

  private def renderEmptyCells(grid: HexagonGrid, buffer: RenderBuffer): Unit = {
    // TODO : better way to iterate (filter?)
    grid.cells.foreach(cell => {
      cell.content match {
        case Empty => {
          renderEmptyCell(buffer.forCell(cell))
        }
        case _ => ()
      }
    })
  }

  private def renderEmptyCell(renderBuffer: CellRenderBuffer): Unit = {
    renderBuffer.setLine1(2, "__", AnsiColors.ANSI_BLUE)
    renderBuffer.setLine2(1, "/~~\\", AnsiColors.ANSI_BLUE)
    renderBuffer.setLine3(1, "\\__/", AnsiColors.ANSI_BLUE)
  }

  private def renderShapes(grid: HexagonGrid, buffer: RenderBuffer): Unit = {
    // TODO : better way to iterate (filter?)
    grid.cells.foreach(cell => {
      cell.content match {
        case ShapeSegment(openSides) => renderShape(buffer.forCell(cell), openSides)
        case _ => ()
      }
    })
  }

  private def renderShape(renderBuffer: CellRenderBuffer, openSides: Seq[HexaDirection]): Unit = {
    renderBuffer.setLine2(2, "  ", AnsiColors.ANSI_WHITE)
    if (!openSides.contains(UpLeft)) renderBuffer.setLine2(1, "/", AnsiColors.ANSI_WHITE)
    if (!openSides.contains(Up)) renderBuffer.setLine1(2, "__", AnsiColors.ANSI_WHITE)
    if (!openSides.contains(UpRight)) renderBuffer.setLine2(4, "\\", AnsiColors.ANSI_WHITE)
    if (!openSides.contains(DownLeft)) renderBuffer.setLine3(1, "\\", AnsiColors.ANSI_WHITE)
    if (!openSides.contains(Down)) renderBuffer.setLine3(2, "__", AnsiColors.ANSI_WHITE)
    if (!openSides.contains(DownRight)) renderBuffer.setLine3(4, "/", AnsiColors.ANSI_WHITE)
  }

  private def renderPawns(grid: HexagonGrid, buffer: RenderBuffer): Unit = {
    // TODO : better way to iterate (filter?)
    grid.cells.foreach(cell => {
      cell.content match {
        case Pawn() => renderPawn(buffer.forCell(cell))
        case _ => ()
      }
    })
  }

  private def renderPawn(renderBuffer: CellRenderBuffer): Unit = {
    renderBuffer.setLine1(2, "__", AnsiColors.ANSI_PURPLE)
    renderBuffer.setLine2(1, "/", AnsiColors.ANSI_PURPLE)
    renderBuffer.setLine2(2, "oo", AnsiColors.ANSI_YELLOW)
    renderBuffer.setLine2(4, "\\", AnsiColors.ANSI_PURPLE)
    renderBuffer.setLine3(1, "\\__/", AnsiColors.ANSI_PURPLE)
  }
}

private case class CharDimensions(width: Int, height: Int) {
  // add one for the line line
  def widthWithLineBreak: Int = width + 1
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

  def setLine1(offset: Int, str: String, color: String): Unit = {
    setLineX(0, offset, str, color)
  }

  def setLine2(offset: Int, str: String, color: String): Unit = {
    setLineX(1, offset, str, color)
  }

  def setLine3(offset: Int, str: String, color: String): Unit = {
    setLineX(2, offset, str, color)
  }

  private def setLineX(lineOffset: Int, colOffset: Int, str: String, color: String): Unit = {
    val line = adjustLine(cell.x, cell.y) + lineOffset
    val col = (cell.x * 3) + colOffset
    for (i <- 0 until str.length) {
      // TODO : skip ' ' (or merge it?)
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
