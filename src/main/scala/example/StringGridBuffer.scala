package example

import scala.collection.mutable.ArrayBuffer

/**
  * Stores character strings in a grid.
  */
trait StringGridBuffer {

  /**
    * Writes a string at the given position.
    *
    * @param line   the line at which to write the string (indexed from 0)
    * @param column the column at which to start writing the string (indexed from 0)
    * @param str    the string
    */
  def write(line: Int, column: Int, str: String): Unit = {
    write(line, column, str, Nil)
  }

  /**
    * Writes a string at the given position. This method takes an additional option handled by the implementation.
    *
    * @param line   the line at which to write the string (indexed from 0)
    * @param column the column at which to start writing the string (indexed from 0)
    * @param str    the string
    * @param option an additional option
    */
  def write(line: Int, column: Int, str: String, option: Any): Unit

  /**
    * Returns the content of the buffer at a single string.
    *
    * @param separator the line separator (defaults to \n)
    * @return a string
    */
  def asString(separator: String = s"\n"): String
}

/**
  * Base implementation of {StringGridBuffer}.
  */
private class BaseStringGridBuffer(buffer: ArrayBuffer[ArrayBuffer[String]]) extends StringGridBuffer {

  override final def write(line: Int, col: Int, str: String, option: Any): Unit =
    (0 until str.length).foreach(i => write(line, col + i, str.charAt(i), option))

  def write(line: Int, col: Int, c: Char, option: Any): Unit =
    buffer(line)(col) = c.toString

  override final def asString(separator: String): String = {
    val stringBuilder = new StringBuilder
    buffer foreach (line => {
      line foreach (stringBuilder ++= _)
      stringBuilder ++= separator
    })
    stringBuilder.result
  }
}

/**
  * An implementation of {StringGridBuffer} that can render a string in color.
  */
private class ColorStringGridBuffer(buffer: ArrayBuffer[ArrayBuffer[String]]) extends BaseStringGridBuffer(buffer) {

  override def write(line: Int, col: Int, c: Char, option: Any): Unit =
    buffer(line)(col) = s"$option$c${AnsiColors.ANSI_RESET}"
}

object StringGridBuffer {

  def default(width: Int, height: Int): StringGridBuffer = {
    val lineBuffer = createStringBuffer(width, height)
    new BaseStringGridBuffer(lineBuffer)
  }

  def color(width: Int, height: Int): StringGridBuffer = {
    val lineBuffer = createStringBuffer(width, height)
    new ColorStringGridBuffer(lineBuffer)
  }

  private def createStringBuffer(width: Int, height: Int): ArrayBuffer[ArrayBuffer[String]] = {
    val buffer = new ArrayBuffer[ArrayBuffer[String]](height)
    for (_ <- 0 until height) {
      val lineBuffer = new ArrayBuffer[String](width)
      buffer += lineBuffer
      for (_ <- 0 until width) {
        lineBuffer += " "
      }
    }
    buffer
  }
}
