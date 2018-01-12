package example

sealed trait CellContent

/**
  * Represents a pawn in a game
  */
case class Pawn() extends CellContent

/**
  * Represents a segment in a complex shape.
  *
  * @param openSides the direction of th open sides 
  */
case class ShapeSegment(openSides: Seq[HexaDirection] = Seq.empty) extends CellContent
