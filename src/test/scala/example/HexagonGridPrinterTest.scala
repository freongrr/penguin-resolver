package example

import example.HexaDirections._
import org.scalatest.FunSuite

class HexagonGridPrinterTest extends FunSuite {

  test("test empty 2x2 grid") {
    val grid = new HexagonGrid(2, 2)
    assert(renderGrid(grid) == "" +
      "  __     \n" +
      " /  \\__  \n" +
      " \\__/  \\ \n" +
      " /  \\__/ \n" +
      " \\__/  \\ \n" +
      "    \\__/ \n")
  }

  test("test 3x3 with shape") {
    val grid = new HexagonGrid(3, 2)
    grid += (0, 0, Shape(Seq(DownRight, UpRight)))

    assert(renderGrid(grid) == "" +
      "  __    __  \n" +
      " /S \\__/S \\ \n" +
      " \\__ S  __/ \n" +
      " /  \\__/  \\ \n" +
      " \\__/  \\__/ \n" +
      "    \\__/    \n")
  }

  private def renderGrid(grid: HexagonGrid) = {
    val printer = new HexagonGridPrinter
    printer.render(grid)
  }
}
