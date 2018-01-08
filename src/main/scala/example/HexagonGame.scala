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
    if (testShape(shape, (x, y) => isHexagonFree(x, y))) {
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
    if (x < 0 || x >= grid.width || y < 0 || y >= grid.height) {
      println("outside of grid")
      false
    } else {
      shapes.forall(s => {
        testShape(s, (sx, sy) => {
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

  def visitShape(shape: Shape, function: (Int, Int) => Unit): Unit = {
    testShape(shape, (x, y) => {
      function(x, y)
      true
    })
  }

  def testShape(shape: Shape, predicate: (Int, Int) => Boolean): Boolean = {
    var x = shape.x
    var y = shape.y
    // Run the predicate first with the initial coordinates
    predicate(x, y) &&
      shape.directions.forall(d => {
        // TODO : assign both variables?
        val newPosition = updatePosition(x, y, d)
        x = newPosition._1
        y = newPosition._2
        predicate(x, y)
      })
  }

  private def updatePosition(x: Int, y: Int, direction: HexaDirection): (Int, Int) = {
    var (x2, y2) = (x, y)

    // whether we change row when going up/down depends on the column and whether we shift up or down
    // e.g. when shifting odd columns down, we use UpRight and then DownRight while staying on row 1
    //     __    __    __  
    //    /  \__/2 \__/  \ 
    //    \__/1 \__/3 \__/ 
    //    /  \__/  \__/  \ 
    //    \__/  \__/  \__/  

    direction match {
      case Up => y2 -= 1
      case UpLeft | UpRight => {
        if (x2 % 2 == 0 == this.grid.shiftOddDown) {
          y2 -= 1
        }
      }
      case Down => y2 += 1
      case DownLeft | DownRight => {
        if (x2 % 2 == 0 != this.grid.shiftOddDown) {
          y2 += 1
        }
      }
    }

    direction match {
      case UpRight | DownRight => x2 += 1
      case UpLeft | DownLeft => x2 -= 1
      case _ => ()
    }

    (x2, y2)
  }
}

object HexagonGameMain {
  def main(args: Array[String]): Unit = {
    val printer = new HexagonGamePrinter

    val grid = new HexagonGrid(4, 3)
    val game = new HexagonGame(grid)
    game += Shape(0, 0, Seq(DownRight, UpRight, DownRight, Down, Down, UpLeft, DownLeft, UpLeft, Up))

    print(printer.render(game))
  }
}
