import org.scalajs.dom.html.Canvas
import org.scalajs.dom.{CanvasRenderingContext2D, document}

object CanvasGridRenderer {

  def main(args: Array[String]): Unit = {
    // TODO : create the canvas here?
    val canvas = document.getElementById("canvas").asInstanceOf[Canvas]
    val ctx = canvas.getContext("2d").asInstanceOf[CanvasRenderingContext2D]

    // val grid = HexagonGrid(5, 4, shiftOddDown = false)

    val hMargin = 20
    val vMargin = 40
    val hexaWidth = 40
    val hexaHeight = 36

    for (y <- 0 until 4 /* grid.height */ ; x <- 0 until 5 /* grid.width */ ) {
      println(s"x: $x, y: $y")
      // val c = grid(x, y)
      val left = x match {
        case 0 => hMargin
        case _ => hMargin + ((hexaWidth * 0.75) * x).intValue()
      }
      val top = x % 2 match {
        case 0 => vMargin + (hexaHeight * y).intValue()
        case 1 => vMargin + (hexaHeight * (y - 0.5)).intValue()
      }
      println(s" > left: $left, top: $top")
      drawHexa(ctx, left, top, hexaWidth, hexaHeight)
    }
  }

  // Base shape & positions
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
  private def drawHexa(ctx: CanvasRenderingContext2D, x: Int, y: Int, width: Int, height: Int): Unit = {
    val left = x
    val top = y
    val leftQuarter = x + width * 0.25
    val rightQuarter = x + width * 0.75
    val right = x + width
    val middle = y + height / 2
    val bottom = y + height

    ctx.strokeStyle = "rgb(255, 0, 0)"
    ctx.lineWidth = 2

    ctx.moveTo(leftQuarter, top)
    ctx.lineTo(rightQuarter, top)
    ctx.lineTo(right, middle)
    ctx.lineTo(rightQuarter, bottom)
    ctx.lineTo(leftQuarter, bottom)
    ctx.lineTo(left, middle)
    ctx.lineTo(leftQuarter, top)
    ctx.stroke()
  }
}
