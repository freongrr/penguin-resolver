import com.github.freongrr.penguinresolver.grid.HexaDirections._
import com.github.freongrr.penguinresolver.grid._
import org.scalajs.dom.CanvasRenderingContext2D
import org.scalajs.dom.html.Canvas

private class CanvasGridRenderer(canvas: Canvas) extends GridRenderer {

  private val CellComparator = Ordering.by[Cell, Int] {
    case EmptyCell(_, _) => 1
    case OccupiedCell(_, _, content) => content match {
      case Pawn() => 2
      case ShapeSegment(_) => 3
    }
  }

  private val HexaWidth = 40
  private val HexaHeight = 36

  private var _grid: HexagonGrid = HexagonGrid(0, 0)
  private var horizontalMargin = 0
  private var verticalMargin = 0

  override def grid: HexagonGrid = _grid

  override def grid_=(newGrid: HexagonGrid): Unit = {
    this._grid = newGrid
    render()
  }

  override def render(): Unit = {
    val ctx = canvas.getContext("2d").asInstanceOf[CanvasRenderingContext2D]
    ctx.clearRect(0, 0, canvas.width, canvas.height)

    horizontalMargin = ((canvas.width - HexaWidth - ((grid.width - 1) * HexaWidth * 0.75)) / 2).intValue()
    verticalMargin = HexaHeight / 4 + ((canvas.height - grid.height * HexaHeight) / 2).intValue()

    // Render cells sorted by depth
    grid.cells sorted CellComparator foreach (renderCell(ctx, grid, _))
  }

  override def getCellAt(x: Int, y: Int): Option[Cell] = {
    grid.cells.find(c => {
      val bounds = getCellBounds(c.y, c.x)
      x >= bounds.leftQuarter && x <= bounds.rightQuarter && y >= bounds.top && y <= bounds.bottom
    })
  }

  private def renderCell(ctx: CanvasRenderingContext2D, grid: HexagonGrid, cell: Cell): Unit = {
    val bounds = getCellBounds(cell.y, cell.x)
    // println(s"Rendering: $cell at $bounds")

    cell match {
      case EmptyCell(_, _) =>
        drawBackground(ctx, bounds, "hsl(200, 100%, 50%)")
        drawSides(ctx, bounds, Seq.empty, "hsl(200, 100%, 75%)", 1)
      case OccupiedCell(_, _, content) =>
        content match {
          case Pawn() =>
            drawBackground(ctx, bounds, "rgb(255, 0, 255)")
            drawSides(ctx, bounds, Seq.empty, "rgb(0, 0, 0)", 1)
          case ShapeSegment(openSides) =>
            drawBackground(ctx, bounds, "rgb(192, 192, 192)")
            drawSides(ctx, bounds, openSides, "rgb(0, 0, 0)", 1)
        }
    }
  }

  /**
    * Returns the bounds of the cell at the given coordinates. 
    *
    * @param row    the row of the cell
    * @param column the column of the cell
    * @return an instance of HexaBounds
    */
  private def getCellBounds(row: Int, column: Int): HexaBounds = {

    // Hexa cell bounds
    //
    // x0: margin
    // x1: margin + width * 0.75 
    // x2: margin + width * 0.75 + width * 0.75
    // ...
    // y0 (even col): margin
    // y1 (even col): margin + height
    // y2 (even col): margin + height + height
    // ...
    // y0 (odd col): margin - height / 2
    // y1 (odd col): margin - height / 2 + height
    // y2 (odd col): margin - height / 2 + height + height
    // ...
    // 
    //      __
    //   __/  \__
    //  /  \__/  \
    //  \__/  \__/
    //  /  \__/  \
    //  \__/  \__/
    //  /  \__/  \
    //  \__/  \__/
    //

    val left = column match {
      case 0 => horizontalMargin
      case _ => horizontalMargin + (HexaWidth * 0.75) * column
    }
    // TODO : this should look at grid.shiftOddDown!
    val top = column % 2 match {
      case 0 => verticalMargin + (HexaHeight * row)
      case 1 => verticalMargin + (HexaHeight * (row - 0.5))
    }
    HexaBounds(left, top, HexaWidth, HexaHeight)
  }

  private def drawBackground(ctx: CanvasRenderingContext2D, bounds: HexaBounds, color: String): Unit = {
    ctx.save()
    ctx.fillStyle = color
    ctx.beginPath()
    ctx.moveTo(bounds.leftQuarter, bounds.top)
    ctx.lineTo(bounds.rightQuarter, bounds.top)
    ctx.lineTo(bounds.right, bounds.middle)
    ctx.lineTo(bounds.rightQuarter, bounds.bottom)
    ctx.lineTo(bounds.leftQuarter, bounds.bottom)
    ctx.lineTo(bounds.left, bounds.middle)
    ctx.lineTo(bounds.leftQuarter, bounds.top)
    ctx.closePath()
    ctx.fill()
    ctx.restore()
  }

  private def drawSides(ctx: CanvasRenderingContext2D, bounds: HexaBounds, openSides: Seq[HexaDirection], lineColor: String, lineWidth: Int): Unit = {
    def drawLine(x1: Double, y1: Double, x2: Double, y2: Double): Unit = {
      ctx.save()
      ctx.strokeStyle = lineColor
      ctx.lineWidth = lineWidth
      ctx.beginPath()
      ctx.moveTo(x1, y1)
      ctx.lineTo(x2, y2)
      ctx.closePath()
      ctx.stroke()
      ctx.restore()
    }

    if (!openSides.contains(UpLeft)) drawLine(bounds.left, bounds.middle, bounds.leftQuarter, bounds.top)
    if (!openSides.contains(Up)) drawLine(bounds.leftQuarter, bounds.top, bounds.rightQuarter, bounds.top)
    if (!openSides.contains(UpRight)) drawLine(bounds.rightQuarter, bounds.top, bounds.right, bounds.middle)
    if (!openSides.contains(DownLeft)) drawLine(bounds.leftQuarter, bounds.bottom, bounds.left, bounds.middle)
    if (!openSides.contains(Down)) drawLine(bounds.rightQuarter, bounds.bottom, bounds.leftQuarter, bounds.bottom)
    if (!openSides.contains(DownRight)) drawLine(bounds.right, bounds.middle, bounds.rightQuarter, bounds.bottom)
  }

  private case class HexaBounds(x: Double, y: Double, width: Double, height: Double) {

    def left: Double = x

    def top: Double = y

    def leftQuarter: Double = x + width * 0.25

    def rightQuarter: Double = x + width * 0.75

    def right: Double = x + width

    def middle: Double = y + height / 2

    def bottom: Double = y + height
  }

}

object CanvasGridRenderer {

  def apply(canvas: Canvas, grid: HexagonGrid): GridRenderer = {
    val renderer = new CanvasGridRenderer(canvas)
    renderer._grid = grid
    renderer
  }
}
