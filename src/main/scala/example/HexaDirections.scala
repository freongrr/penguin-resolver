package example

sealed trait HexaDirection

object HexaDirections {

  case object UpRight extends HexaDirection

  case object Up extends HexaDirection

  case object UpLeft extends HexaDirection

  case object DownLeft extends HexaDirection

  case object Down extends HexaDirection

  case object DownRight extends HexaDirection

}
