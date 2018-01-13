package com.github.freongrr.penguinresolver.grid

/**
  * Represents a direction in an hexagonal grid.
  */
sealed trait HexaDirection {

  /**
    * Returns the opposite direction. This is the equivalent of calling #rotateByOne 3 times.
    *
    * @return a direction
    */
  def opposite: HexaDirection

  /**
    * Rotates the direction counter-clockwise. 
    *
    * @return a direction
    */
  def rotateByOne(): HexaDirection
}

object HexaDirections {

  case object UpRight extends HexaDirection {
    override def opposite: HexaDirection = DownLeft
    override def rotateByOne(): HexaDirection = Up
  }

  case object Up extends HexaDirection {
    override def opposite: HexaDirection = Down
    override def rotateByOne(): HexaDirection = UpLeft
  }

  case object UpLeft extends HexaDirection {
    override def opposite: HexaDirection = DownRight
    override def rotateByOne(): HexaDirection = DownLeft
  }

  case object DownLeft extends HexaDirection {
    override def opposite: HexaDirection = UpRight
    override def rotateByOne(): HexaDirection = Down
  }

  case object Down extends HexaDirection {
    override def opposite: HexaDirection = Up
    override def rotateByOne(): HexaDirection = DownRight
  }

  case object DownRight extends HexaDirection {
    override def opposite: HexaDirection = UpLeft
    override def rotateByOne(): HexaDirection = UpRight
  }
}
