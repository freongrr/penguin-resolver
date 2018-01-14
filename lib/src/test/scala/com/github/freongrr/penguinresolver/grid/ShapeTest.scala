package com.github.freongrr.penguinresolver.grid

import com.github.freongrr.penguinresolver.grid.HexaDirections._
import org.scalatest.FunSuite

class ShapeTest extends FunSuite {

  test("first position of empty shape is closed shape segment") {
    val shape = Shape()
    assert(shape(0) == ShapeSegment())
  }

  test("can't get shape outside of bounds (1)") {
    val shape = Shape(Seq(Down))
    assertThrows[IllegalArgumentException] {
      shape(-1)
    }
  }

  test("can't get shape outside of bounds (2)") {
    val shape = Shape(Seq(Down))
    assertThrows[IllegalArgumentException] {
      shape(5)
    }
  }

  test("first position shape going down is open shape going down") {
    val shape = Shape(Seq(Down))
    assert(shape(0) == ShapeSegment(Seq(Down)))
  }

  test("last position shape going down is open shape going up") {
    val shape = Shape(Seq(Down))
    assert(shape(1) == ShapeSegment(Seq(Up)))
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
    assert(shape(0) == ShapeSegment(Seq(Up)))
    assert(shape(1) == ShapeSegment(Seq(Down, UpRight)))
    assert(shape(2) == ShapeSegment(Seq(DownLeft, DownRight)))
    assert(shape(3) == ShapeSegment(Seq(UpLeft)))
  }

  test("empty shape rotate equals to itself") {
    val shape = Shape()
    assert(shape.rotate(1) == Shape())
  }

  test("simple positive rotation") {
    val shape = Shape(Seq(Down))
    assert(shape.rotate(1) == Shape(Seq(DownRight)))
  }

  test("simple negative rotation") {
    val shape = Shape(Seq(Down))
    assert(shape.rotate(-1) == Shape(Seq(DownLeft)))
  }

  test("complex shape rotation") {
    // | | becomes / \
    // \ /         | |
    val shape = Shape(Seq(Down, DownRight, UpRight, Up))
    assert(shape.rotate(3) == Shape(Seq(Up, UpLeft, DownLeft, Down)))
  }
}
