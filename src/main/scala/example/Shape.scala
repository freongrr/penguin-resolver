package example

/**
  * Represents a shape in an hexagonal grid. The shape is expressed as a sequence of direction changes (a empty
  * sequence means a single cell).
  */
case class Shape(directions: Seq[HexaDirection] = Seq.empty) {

  def length: Int = directions.length


  // TODO : try with a method like that:
  // def segments: Seq[ShapeSegment] = {
  //   0 to length map (i => at(i))
  // }

  def at(index: Int): ShapeSegment = {
    if (index < 0 || index > length)
      throw new IllegalArgumentException(s"x is outside bounds [0:$length]")

    if (index == 0) {
      if (length != 0) {
        ShapeSegment(Seq(directions.head))
      } else {
        ShapeSegment()
      }
    } else if (index == length) {
      ShapeSegment(Seq(directions(index - 1).opposite))
    } else {
      ShapeSegment(Seq(
        directions(index - 1).opposite,
        directions(index)))
    }
  }

  // TODO : add a method that rotates the shape and returns a new shape
}
