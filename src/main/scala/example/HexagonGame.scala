package example

import example.HexaDirections._

import scala.util.{Failure, Success, Try}

/**
  * TODO : documentation
  *
  * @author fcortina
  * @since 03/01/2018 - 22:50
  */
class HexagonGame(val grid: HexagonGrid) {

  private val _shapes = new scala.collection.mutable.ArrayBuffer[Shape]

  def shapes: Seq[Shape] = _shapes

  def +=(shape: Shape): Try[Unit] = {
    if (shape.test((x, y) => isHexagonFree(x, y))) {
      println(s"Adding shape $shape")
      _shapes += shape
      Success()
    } else {
      println(s"Can't add shape $shape")
      Failure(new IllegalArgumentException("Can't add shape here"))
    }
  }

  def isHexagonFree(x: Int, y: Int): Boolean = {
    println(s"Testing position: $x, $y")
    if (x < 0 || x > grid.width || y < 0 || y > grid.height) {
      println("outside of grid")
      false
    } else {
      shapes.forall(s => {
        s.test((sx, sy) => {
          if (sx == x && sy == y) {
            println(s"intersect with shape $s")
            false
          } else {
            true
          }
        })
      })
    }
  }
}

object HexagonGameMain {
  def main(args: Array[String]): Unit = {
    val printer = new HexagonGamePrinter

    val game1 = new HexagonGame(HexagonGrid(3, 3))

    // Outside:
    game1 += Shape(0, 0, Seq(HexaDirections.Up))

    // 1x1 shape ok
    game1 += Shape(1, 0, Seq())

    // 1x2 shape ok
    game1 += Shape(2, 0, Seq(HexaDirections.Down, HexaDirections.Down))

    // 2x1 shape overlapping with other shape 
    game1 += Shape(0, 0, Seq(HexaDirections.DownRight))

    val game2 = new HexagonGame(HexagonGrid(5, 5))
    game2 += Shape(1, 1, Seq(UpRight, DownRight, Down, DownLeft, UpLeft))

    print(printer.render(game2))
  }
}
