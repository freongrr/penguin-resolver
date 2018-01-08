package example

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

  test("add a Pawn in the grid") {
    val grid = new HexagonGrid(4, 3)

    val pawn = new DummyPawn
    assert((grid += (0, 0, pawn)).isSuccess)
  }

  test("can't add a Pawn in an occupied cell") {
    val grid = new HexagonGrid(4, 3)

    assert((grid += (0, 0, new DummyPawn)).isSuccess)
    assert((grid += (0, 0, new DummyPawn)).isFailure)
  }
}
