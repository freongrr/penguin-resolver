package example

case class Shape(x: Int = 0, y: Int = 0, directions: Seq[HexaDirection] = Seq.empty) {

  def length: Int = directions.length

  def visit(function: (Int, Int) => Unit): Unit = {
    this.test((xx, yy) => {
      function(xx, yy)
      true
    })
  }

  def test(predicate: (Int, Int) => Boolean): Boolean = {
    var xx = this.x
    var yy = this.y
    // Run the predicate first with the initial coordinates
    predicate(xx, yy) &&
      this.directions.forall(d => {
        // TODO : whether we change row when going up/down depends on the column and whether we shift up or down
        // e.g. when shifting odd columns down, we use UpRight and then DownRight while staying on row 1
        //     __    __    __  
        //    /  \__/2 \__/  \ 
        //    \__/1 \__/3 \__/ 
        //    /  \__/  \__/  \ 
        //    \__/  \__/  \__/  

        d match {
          case HexaDirections.UpRight => xx += 1; yy -= 1
          case HexaDirections.Up => yy -= 1
          case HexaDirections.UpLeft => xx -= 1; yy -= 1
          case HexaDirections.DownLeft => xx -= 1; yy += 1
          case HexaDirections.Down => yy += 1
          case HexaDirections.DownRight => xx += 1; yy += 1
        }
        predicate(xx, yy)
      })
  }
}
