package example

/**
  * TODO : Use different styles for the background and shapes
  */
class HexagonGamePrinter {

  /**
    * Renders a HexagonGame to a StringBuilder.
    *
    * @param game the game to render
    * @return a StringBuilder filled with a string representation of the game
    */
  def render(game: HexagonGame): StringBuilder = {
    val dimensions = charBufferDimensions(game.grid)
    val builder = createBuilder(dimensions)

    renderGrid(builder, game.grid, dimensions)
    renderShapes(builder, game, dimensions)

    builder
  }

  private def renderGrid(builder: StringBuilder, grid: HexagonGrid, dimensions: CharDimensions): Unit = {
    for (y <- 0 until grid.height) {
      for (x <- 0 until grid.width) {
        renderHexagon(builder, grid, dimensions, x, y, ' ')
      }
    }
  }

  // TODO : reduce number of parameters!
  private def renderHexagon(builder: StringBuilder, grid: HexagonGrid, dimensions: CharDimensions, x: Int, y: Int, content: Char): Unit = {
    val line1 = hexagonOffset(x, y, dimensions, grid.shiftOddDown)
    val line2 = line1 + dimensions.widthWithLineBreak
    val line3 = line2 + dimensions.widthWithLineBreak
    builder.setCharAt(line1 + 2, '_')
    builder.setCharAt(line1 + 3, '_')
    builder.setCharAt(line2 + 1, '/')
    builder.setCharAt(line2 + 2, content)
    builder.setCharAt(line2 + 4, '\\')
    builder.setCharAt(line3 + 1, '\\')
    builder.setCharAt(line3 + 2, '_')
    builder.setCharAt(line3 + 3, '_')
    builder.setCharAt(line3 + 4, '/')
  }

  private def renderShapes(builder: StringBuilder, game: HexagonGame, dimensions: CharDimensions): Unit = {
    // TODO : erase borders when moving between hexagons!
    var c = 0
    for (s <- game.shapes) {
      game.visitShape(s, (x, y) => {
        renderHexagon(builder, game.grid, dimensions, x, y, c.toString.charAt(0))
        c += 1
      })
    }
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

private case class CharDimensions(width: Int, height: Int) {
  // add one for the line line
  def widthWithLineBreak: Int = width + 1
}
