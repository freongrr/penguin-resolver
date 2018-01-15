import com.github.freongrr.penguinresolver.grid.HexaDirections._
import com.github.freongrr.penguinresolver.grid._
import org.scalajs.dom.CanvasRenderingContext2D
import org.scalajs.dom.html.Canvas

object CanvasGridRenderer {

  private val HorizontalMargin = 20
  private val VerticalMargin = 40
  private val HexaWidth = 40
  private val HexaHeight = 36

  def render(canvas: Canvas, grid: HexagonGrid): Unit = {
    val ctx = canvas.getContext("2d").asInstanceOf[CanvasRenderingContext2D]

    for (y <- 0 until grid.height; x <- 0 until grid.width) {
      val cell = grid(x, y)
      val bounds = getCellBounds(y, x)
      println(s"Rendering: $cell at left=${bounds.left}, top=${bounds.top}")

      cell match {
        case EmptyCell(_, _) => {
          drawHexa(ctx, bounds, "rgb(0, 0, 255)")
        }
        case OccupiedCell(_, _, content) => {
          content match {
            case Pawn() => {
              drawHexa(ctx, bounds, "rgb(255, 0, 255)")
            }
            case ShapeSegment(openSides) => {
              drawBackground(ctx, bounds, "rgb(128, 128, 128)")
              drawShapeSides(ctx, bounds, openSides)
            }
          }
        }
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
      case 0 => HorizontalMargin
      case _ => HorizontalMargin + (HexaWidth * 0.75) * column
    }
    // TODO : this should look at grid.shiftOddDown!
    val top = column % 2 match {
      case 0 => VerticalMargin + (HexaHeight * row)
      case 1 => VerticalMargin + (HexaHeight * (row - 0.5))
    }
    HexaBounds(left, top, HexaWidth, HexaHeight)
  }

  private def drawHexa(ctx: CanvasRenderingContext2D, bounds: HexaBounds, color: String): Unit = {
    ctx.save()
    ctx.strokeStyle = "rgb(0, 0, 0)"
    ctx.fillStyle = color
    ctx.lineWidth = 2
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
    ctx.stroke()
    ctx.restore()
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

  private def drawShapeSides(ctx: CanvasRenderingContext2D, bounds: HexaBounds, openSides: Seq[HexaDirection]): Unit = {
    def drawLine(x1: Double, y1: Double, x2: Double, y2: Double): Unit = {
      ctx.save()
      ctx.strokeStyle = "rgb(0, 0, 0)"
      ctx.lineWidth = 2
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
    if (!openSides.contains(DownLeft)) drawLine(bounds.right, bounds.middle, bounds.rightQuarter, bounds.bottom)
    if (!openSides.contains(Down)) drawLine(bounds.rightQuarter, bounds.bottom, bounds.leftQuarter, bounds.bottom)
    if (!openSides.contains(DownRight)) drawLine(bounds.leftQuarter, bounds.bottom, bounds.left, bounds.middle)
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
