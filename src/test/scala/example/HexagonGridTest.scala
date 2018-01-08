package example

import example.HexaDirections._
import org.scalatest.FunSuite

class HexagonGridTest extends FunSuite {

  test("all cells of grid are empty") {
    val grid = new HexagonGrid(3, 2)

    assert(grid(0, 0) == Cell(0, 0))
    assert(grid(1, 0) == Cell(1, 0))
    assert(grid(2, 0) == Cell(2, 0))
    assert(grid(0, 1) == Cell(0, 1))
    assert(grid(1, 1) == Cell(1, 1))
    assert(grid(2, 1) == Cell(2, 1))
  }

  test("add a Pawn inside the grid") {
    val grid = new HexagonGrid(4, 3)

    assert((grid += (0, 0, Pawn())).isSuccess)
  }

  test("can't add a Pawn outside of the grid") {
    val grid = new HexagonGrid(4, 3)

    assert((grid += (-1, -1, Pawn())).isFailure)
    assert((grid += (10, 10, Pawn())).isFailure)
  }

  test("can't add a Pawn in an occupied cell") {
    val grid = new HexagonGrid(4, 3)

    assert((grid += (0, 0, Pawn())).isSuccess)
    assert((grid += (0, 0, Pawn())).isFailure)
  }

  test("add a shape inside an empty grid") {
    val grid = new HexagonGrid(4, 3)

    val shape = Shape(Seq(Down))
    assert((grid += (0, 0, shape)).isSuccess)
  }

  test("can't add a shape that continues outside of the grid") {
    val grid = new HexagonGrid(4, 3)

    val shape = Shape(Seq(Down, Down, Down, Down))
    assert((grid += (0, 0, shape)).isFailure)
  }

  test("encircle grid with default shift") {
    val grid = new HexagonGrid(4, 3)
    assert((grid += (0, 0, Shape(Seq(DownRight, UpRight, DownRight, Down, Down, UpLeft, DownLeft, UpLeft, Up)))).isSuccess)
  }

  test("encircle grid with up shift") {
    val grid = new HexagonGrid(4, 3, shiftOddDown = false)
    assert((grid += (0, 0, Shape(Seq(UpRight, DownRight, UpRight, Down, Down, DownLeft, UpLeft, DownLeft, Up)))).isSuccess)
  }
}
