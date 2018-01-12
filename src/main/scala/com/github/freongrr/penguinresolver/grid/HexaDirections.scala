package com.github.freongrr.penguinresolver.grid

/**
  * Represents a direction in an hexagonal grid
  */
sealed trait HexaDirection {

  def opposite: HexaDirection =
    this match {
      case HexaDirections.UpRight => HexaDirections.DownLeft
      case HexaDirections.Up => HexaDirections.Down
      case HexaDirections.UpLeft => HexaDirections.DownRight
      case HexaDirections.DownLeft => HexaDirections.UpRight
      case HexaDirections.Down => HexaDirections.Up
      case HexaDirections.DownRight => HexaDirections.UpLeft
    }

  def rotateByOne(): HexaDirection =
    this match {
      case HexaDirections.UpRight => HexaDirections.Up
      case HexaDirections.Up => HexaDirections.UpLeft
      case HexaDirections.UpLeft => HexaDirections.DownLeft
      case HexaDirections.DownLeft => HexaDirections.Down
      case HexaDirections.Down => HexaDirections.DownRight
      case HexaDirections.DownRight => HexaDirections.UpRight
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
