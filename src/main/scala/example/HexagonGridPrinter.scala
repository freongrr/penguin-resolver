package example

/**
  * TODO : Use different styles for the background and shapes
  */
class HexagonGridPrinter {

  /**
    * Renders a HexagonGame to a StringBuilder.
    *
    * @param grid the grid to render
    * @return a StringBuilder filled with a string representation of the game
    */
  def render(grid: HexagonGrid): StringBuilder = {
    val dimensions = charBufferDimensions(grid)
    val builder = createBuilder(dimensions)

    grid.cells foreach renderCell(builder, grid, dimensions)

    builder
  }

  private def renderCell(builder: StringBuilder, grid: HexagonGrid, dimensions: CharDimensions)(cell: Cell): Unit = {
    val context = new RenderContext(builder, grid, dimensions, cell)
    cell.content match {
      case Nil => renderEmptyCell(context)
      case Pawn => renderPawn(context)
      case Shape(directions) => {
        // TODO : how do I get the direction for this cell?
        renderShape(context)
      }
      case _ => // TODO
    }
  }

  private def renderEmptyCell(renderContext: RenderContext): Unit = {
    renderContext.setLine1(2, "__")
    renderContext.setLine2(1, "/  \\")
    renderContext.setLine3(1, "\\__/")
  }

  private def renderPawn(renderContext: RenderContext): Unit = {
    renderContext.setLine1(2, "__")
    renderContext.setLine2(1, "/P \\")
    renderContext.setLine3(1, "\\__/")
  }

  private def renderShape(renderContext: RenderContext): Unit = {
    renderContext.setLine1(2, "__")
    renderContext.setLine2(1, "/S \\")
    renderContext.setLine3(1, "\\__/")
  }

  /**
    * Computes the number of characters required to display the hexagon grid.
    *
    * @param grid a grid 
    * @return a CharDimensions object containing the width and height in characters
    */
  private def charBufferDimensions(grid: HexagonGrid): CharDimensions = {
    if (grid.width == 0 || grid.height == 0) {
      CharDimensions(0, 0)
    } else {
      CharDimensions(charWidth(grid.width), charHeight(grid.width, grid.height))
    }
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

  /**
    * Creates a StringBuilder of the required dimensions
    *
    * @param dimensions the width and height of the buffer in characters
    * @return an empty char character buffer filled with spaces and line separator
    */
  private def createBuilder(dimensions: CharDimensions): StringBuilder = {
    val buffer = new StringBuilder(dimensions.width * dimensions.widthWithLineBreak)
    // TODO : improve initialization
    for (_ <- 0 until dimensions.height) {
      for (_ <- 0 until dimensions.width) {
        buffer += ' '
      }
      buffer += '\n'
    }
    buffer
  }
}

private case class CharDimensions(width: Int, height: Int) {
  // add one for the line line
  def widthWithLineBreak: Int = width + 1
}

private class RenderContext(val builder: StringBuilder, val grid: HexagonGrid, val dimensions: CharDimensions, val cell: Cell) {

  def setLine1(offset: Int, str: String): Unit = {
    setLineX(0, offset, str)
  }

  def setLine2(offset: Int, str: String): Unit = {
    setLineX(1, offset, str)
  }

  def setLine3(offset: Int, str: String): Unit = {
    setLineX(2, offset, str)
  }

  private def setLineX(line: Int, offset: Int, str: String): Unit = {
    val lineStart = hexagonOffset(cell.x, cell.y, dimensions, grid.shiftOddDown) + (line * dimensions.widthWithLineBreak)
    for (i <- 0 until str.length) {
      // TODO : skip ' ' (or merge it?)
      val idx = lineStart + offset + i
      val c = str.charAt(i)
      builder.setCharAt(idx, c)
    }
  }

  /**
    * Returns the position of an hexagon in the char buffer.
    *
    * @param x            the column of the hexagon
    * @param y            the row of the hexagon
    * @param dimensions   the dimensions of the text buffer
    * @param shiftOddDown true to shift hexagons in odd columns down (or up otherwise)
    * @return the offset of the hexagon in the buffer
    */
  private def hexagonOffset(x: Int, y: Int, dimensions: CharDimensions, shiftOddDown: Boolean): Int = {
    val evenColumnOffset = if (shiftOddDown) 0 else 1
    val oddColumnOffset = if (shiftOddDown) 1 else 0
    val line = 2 * y + (if (x % 2 == 0) evenColumnOffset else oddColumnOffset)
    (line * dimensions.widthWithLineBreak) + (x * 3)
  }
}
