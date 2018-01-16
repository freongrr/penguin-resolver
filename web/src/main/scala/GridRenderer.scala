import com.github.freongrr.penguinresolver.grid.{Cell, HexagonGrid}

/**
  * TODO : documentation
  */
trait GridRenderer {

  def grid: HexagonGrid

  def grid_=(grid: HexagonGrid): Unit

  def render(): Unit

  def getCellAt(x: Int, y: Int): Option[Cell]
}
