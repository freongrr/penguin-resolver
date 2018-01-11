package example

import example.HexaDirections._

/**
  * Represents a direction in an hexagonal grid
  */
sealed trait HexaDirection {

  def opposite: HexaDirection = {
    this match {
      case UpRight => DownLeft
      case Up => Down
      case UpLeft => DownRight
      case DownLeft => UpRight
      case Down => Up
      case DownRight => UpLeft
    }
  }

  def rotateByOne(): HexaDirection = {
    this match {
      case UpRight => Up
      case Up => UpLeft
      case UpLeft => DownLeft
      case DownLeft => Down
      case Down => DownRight
      case DownRight => UpRight
    }
  }
}

object HexaDirections {

  case object UpRight extends HexaDirection

  case object Up extends HexaDirection

  case object UpLeft extends HexaDirection

  case object DownLeft extends HexaDirection

  case object Down extends HexaDirection

  case object DownRight extends HexaDirection

}
