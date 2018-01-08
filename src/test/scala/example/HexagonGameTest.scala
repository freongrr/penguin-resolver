package example

import example.HexaDirections._
import org.scalatest.FunSuite

class HexagonGameTest extends FunSuite {

  test("can add a 1x1 shape inside of grid") {
    val game = new HexagonGame(new HexagonGrid(3, 3))
    assert((game += Shape(0, 0)).isSuccess)
    printGame(game)
  }

  test("can't add 1x1 shape outside of grid") {
    val game = new HexagonGame(new HexagonGrid(3, 3))
    assert((game += Shape(-1, -1, Seq())).isFailure)
    printGame(game)
  }

  test("can add two shapes that don't overlap") {
    val game = new HexagonGame(new HexagonGrid(3, 3))

    assert((game += Shape(0, 0, Seq(HexaDirections.Down))).isSuccess)
    assert((game += Shape(1, 1, Seq(HexaDirections.Up))).isSuccess)
    printGame(game)
  }

  test("can't add shape that overlap existing one") {
    val game = new HexagonGame(new HexagonGrid(3, 3))

    assert((game += Shape(0, 0, Seq(HexaDirections.Down))).isSuccess)
    assert((game += Shape(0, 2, Seq(HexaDirections.Up))).isFailure)

    printGame(game)
  }

  test("circle grid with default shift") {
    val grid = new HexagonGrid(4, 3)
    val game = new HexagonGame(grid)
    assert((game += Shape(0, 0, Seq(DownRight, UpRight, DownRight, Down, Down, UpLeft, DownLeft, UpLeft, Up))).isSuccess)

    printGame(game)
  }

  test("circle grid with up shift") {
    val grid = new HexagonGrid(4, 3, shiftOddDown = false)
    val game = new HexagonGame(grid)
    assert((game += Shape(0, 0, Seq(UpRight, DownRight, UpRight, Down, Down, DownLeft, UpLeft, DownLeft, Up))).isSuccess)

    printGame(game)
  }

  private def printGame(game: HexagonGame): Unit = {
    val printer = new HexagonGamePrinter
    print(printer.render(game))
  }
}
