package example

import example.HexaDirections._
import org.scalatest.FunSuite

class HexagonGridTest extends FunSuite {

  test("all cells of grid are empty") {
    val grid = HexagonGrid(3, 2)

    assert(grid(0, 0) == EmptyCell(0, 0))
    assert(grid(1, 0) == EmptyCell(1, 0))
    assert(grid(2, 0) == EmptyCell(2, 0))
    assert(grid(0, 1) == EmptyCell(0, 1))
    assert(grid(1, 1) == EmptyCell(1, 1))
    assert(grid(2, 1) == EmptyCell(2, 1))
  }

  test("add a Pawn inside the grid") {
    val grid = HexagonGrid(4, 3) :+ (0, 0, Pawn())
    assert(grid(0, 0) == OccupiedCell(0, 0, Pawn()))
  }

  test("can't add a Pawn outside of the grid (1)") {
    val grid = HexagonGrid(4, 3)

    assertThrows[IllegalArgumentException] {
      grid :+ (-1, -1, Pawn())
    }
  }

  test("can't add a Pawn outside of the grid (2)") {
    val grid = HexagonGrid(4, 3)

    assertThrows[IllegalArgumentException] {
      grid :+ (10, 10, Pawn())
    }
  }

  test("can't add a Pawn in an occupied cell") {
    val grid = HexagonGrid(4, 3) :+ (0, 0, Pawn())

    assertThrows[IllegalArgumentException] {
      grid :+ (0, 0, Pawn())
    }
  }

  test("add a shape inside an empty grid") {
    HexagonGrid(4, 3) :+ (0, 0, Shape(Seq(Down)))
  }

  test("can't add a shape that continues outside of the grid") {
    val grid = HexagonGrid(4, 3)

    assertThrows[IllegalArgumentException] {
      grid :+ (0, 0, Shape(Seq(Down, Down, Down, Down)))
    }
  }

  test("encircle grid with default shift") {
    val grid = HexagonGrid(4, 3)
    grid :+ (0, 0, Shape(Seq(DownRight, UpRight, DownRight, Down, Down, UpLeft, DownLeft, UpLeft, Up)))
  }

  test("encircle grid with up shift") {
    val grid = HexagonGrid(4, 3, shiftOddDown = false)
    grid :+ (0, 0, Shape(Seq(UpRight, DownRight, UpRight, Down, Down, DownLeft, UpLeft, DownLeft, Up)))
  }
}
