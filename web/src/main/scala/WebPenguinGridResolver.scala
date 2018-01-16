import com.github.freongrr.penguinresolver.PenguinGameResolver
import com.github.freongrr.penguinresolver.grid._
import org.scalajs.dom
import org.scalajs.dom.html.Canvas
import org.scalajs.dom.{MouseEvent, document}

import scala.scalajs.js.annotation.{JSExport, JSExportTopLevel}

@JSExportTopLevel("GridResolver")
object WebPenguinGridResolver {

  private var renderer: GridRenderer = _

  def main(args: Array[String]): Unit = {
    // TODO : create the canvas here?

    // Add a few pawns to test the render
    val grid: HexagonGrid = HexagonGrid(5, 4, shiftOddDown = false) :+
      (0, 0, Pawn()) :+
      (3, 1, Pawn()) :+
      (2, 3, Pawn())

    val canvas = document.getElementById("canvas").asInstanceOf[Canvas]

    renderer = CanvasGridRenderer(canvas, grid)
    renderer.render()
  }

  @JSExport
  def onClick(event: MouseEvent): Unit = {
    val canvas = event.target.asInstanceOf[Canvas]
    val offsetX = (event.clientX - canvas.offsetLeft).intValue()
    val offsetY = (event.clientY - canvas.offsetTop).intValue()
    val maybeCell = renderer.getCellAt(offsetX, offsetY)
    maybeCell match {
      case Some(c@EmptyCell(x, y)) =>
        println(s"Found cell $c at $offsetX, $offsetY")
        renderer.grid = renderer.grid :+ (x, y, Pawn())
      case _ =>
        println(s"No cell at $offsetX, $offsetY")
    }
  }

  @JSExport
  def clearIt(): Unit = {
    renderer.grid = HexagonGrid(5, 4, shiftOddDown = false)
  }

  @JSExport
  def solveIt(): Unit = {
    val resolver = new PenguinGameResolver(renderer.grid)
    val solutionGrids = resolver.resolve()
    solutionGrids.headOption match {
      case Some(g) => renderer.grid = g
      case None => dom.window.alert("No solution")
    }
  }
}
