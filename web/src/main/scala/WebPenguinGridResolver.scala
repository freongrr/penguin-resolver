import com.github.freongrr.penguinresolver.PenguinGameResolver
import com.github.freongrr.penguinresolver.grid._
import org.scalajs.dom.html.Canvas
import org.scalajs.dom.{CanvasRenderingContext2D, document}

import scala.scalajs.js.annotation.{JSExport, JSExportTopLevel}

@JSExportTopLevel("GridResolver")
object WebPenguinGridResolver {

  private val baseGrid: HexagonGrid = HexagonGrid(5, 4, shiftOddDown = false)

  private var grid: HexagonGrid = baseGrid

  def main(args: Array[String]): Unit = {
    // TODO : create the canvas here?

    // Add a few pawns to test the render
    grid = baseGrid :+ (0, 0, Pawn()) :+ (3, 1, Pawn()) :+ (2, 3, Pawn())

    redrawGrid()
  }

  private def redrawGrid(): Unit = {
    val canvas = document.getElementById("canvas").asInstanceOf[Canvas]
    val ctx = canvas.getContext("2d").asInstanceOf[CanvasRenderingContext2D]
    ctx.clearRect(0, 0, canvas.width, canvas.height)
    CanvasGridRenderer.render(canvas, grid)
  }

  @JSExport
  def clearIt(): Unit = {
    grid = HexagonGrid(5, 4, shiftOddDown = false)
    redrawGrid()
  }

  @JSExport
  def solveIt(): Unit = {
    val resolver = new PenguinGameResolver(grid)
    val solutionGrids = resolver.resolve()
    grid = solutionGrids.head
    redrawGrid()
  }
}
