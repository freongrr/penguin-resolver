import com.github.freongrr.penguinresolver.PenguinGameResolver
import com.github.freongrr.penguinresolver.grid._
import org.scalajs.dom.document
import org.scalajs.dom.html.Canvas

object WebPenguinGridResolver {

  def main(args: Array[String]): Unit = {
    // TODO : create the canvas here?
    val canvas = document.getElementById("canvas").asInstanceOf[Canvas]

    val baseGrid = HexagonGrid(5, 4, shiftOddDown = false) :+
      (0, 0, Pawn()) :+
      (3, 1, Pawn()) :+
      // (1, 2, Pawn()) :+
      (2, 3, Pawn())

    val resolver = new PenguinGameResolver(baseGrid)
    val solutionGrids = resolver.resolve()
    val grid = solutionGrids.head

    CanvasGridRenderer.render(canvas, grid)
  }
}
