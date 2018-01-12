package example

import example.HexaDirections._
import org.scalatest.FunSuite

class HexagonGridPrinterTest extends FunSuite {

  test("test empty 2x2 grid") {
    val grid = HexagonGrid(2, 2)
    assert(renderGrid(grid) == "" +
      "  __     \n" +
      " /~~\\__  \n" +
      " \\__/~~\\ \n" +
      " /~~\\__/ \n" +
      " \\__/~~\\ \n" +
      "    \\__/ \n")
  }

  test("test 3x2 with pawns") {
    val grid = HexagonGrid(3, 2) :+ (0, 0, Pawn()) :+ (2, 1, Pawn())

    assert(renderGrid(grid) == "" +
      "  __    __  \n" +
      " /oo\\__/~~\\ \n" +
      " \\__/~~\\__/ \n" +
      " /~~\\__/oo\\ \n" +
      " \\__/~~\\__/ \n" +
      "    \\__/    \n")
  }

  test("test 3x2 with shape") {
    val grid = HexagonGrid(3, 2) :+ (0, 0, Shape(Seq(DownRight, UpRight)))

    assert(renderGrid(grid) == "" +
      "  __    __  \n" +
      " /  \\__/  \\ \n" +
      " \\__    __/ \n" +
      " /~~\\__/~~\\ \n" +
      " \\__/~~\\__/ \n" +
      "    \\__/    \n")
  }

  private def renderGrid(grid: HexagonGrid) = {
    val printer = new HexagonGridPrinter(StringGridBuffer.default)
    printer.render(grid)
  }
}
