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

    // TODO : collect all potential sides (and sort them)
    //
    // e.g. this shape would give:
    //  __    __
    // /0 \__/4 \ 0: Down, DownRight
    // \__/3 \__/ 1: UpRight, Up, DownRight 
    // /1 \__/    2: Up, UpLeft 
    // \__/2 \    3: UpRight, UpLeft, DownRight, Down
    //    \__/    4: DownLeft

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

  def rotate(i: Int): Shape = {
    Shape(directions.map(d => this.rotate(d, i)))
  }

  private def rotate(direction: HexaDirection, i: Int): HexaDirection = {
    if (i == 0) {
      direction
    } else if (i < 0) {
      rotate(direction, 6 + i)
    } else {
      rotate(direction.rotateByOne(), i - 1)
    }
  }
}
