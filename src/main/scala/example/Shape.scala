package example

/**
  * Represents a shape in an hexagonal grid. The shape is expressed as a sequence of direction changes (a empty
  * sequence means a single cell).
  */
case class Shape(directions: Seq[HexaDirection]) {

  def length: Int = directions.length

  // TODO : add a method that rotates the shape and returns a new shape
}
