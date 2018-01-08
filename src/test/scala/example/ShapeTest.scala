package example

import example.HexaDirections._
import org.scalatest.FunSuite

class ShapeTest extends FunSuite {

  test("first position of empty shape is closed shape segment") {
    val shape = Shape()
    assert(shape.at(0) == ShapeSegment())
  }

  test("can't get shape outside of bounds (1)") {
    val shape = Shape(Seq(Down))
    assertThrows[IllegalArgumentException] {
      shape.at(-1)
    }
  }

  test("can't get shape outside of bounds (2)") {
    val shape = Shape(Seq(Down))
    assertThrows[IllegalArgumentException] {
      shape.at(5)
    }
  }

  test("first position shape going down is open shape going down") {
    val shape = Shape(Seq(Down))
    assert(shape.at(0) == ShapeSegment(Seq(Down)))
  }

  test("last position shape going down is open shape going up") {
    val shape = Shape(Seq(Down))
    assert(shape.at(1) == ShapeSegment(Seq(Up)))
  }

  /* Shape with 2 direction changes:
   * - Up
   * - UpRight
   * - DownRight
   *   __    __ 
   *  /  \__/  \
   *  \__/2 \__/
   *  /1 \__/3 \
   *  \__/  \__/
   *  /0 \__/  \
   *  \__/  \__/
   *     \__/
   *
   * Should give 3 segments:
   * - [Up]
   * - [Down, UpRight]
   * - [DownLeft, DownRight]
   * - [UpLeft]
   */
  test("get segments of a complex shape") {
    val shape = Shape(Seq(Up, UpRight, DownRight))
    assert(shape.at(0) == ShapeSegment(Seq(Up)))
    assert(shape.at(1) == ShapeSegment(Seq(Down, UpRight)))
    assert(shape.at(2) == ShapeSegment(Seq(DownLeft, DownRight)))
    assert(shape.at(3) == ShapeSegment(Seq(UpLeft)))
  }
}
