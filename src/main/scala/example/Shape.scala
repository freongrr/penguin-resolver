package example

case class Shape(x: Int = 0, y: Int = 0, directions: Seq[HexaDirection] = Seq.empty) {

  def length: Int = directions.length
}
