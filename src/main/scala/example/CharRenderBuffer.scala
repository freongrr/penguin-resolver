package example

import scala.collection.mutable.ArrayBuffer

trait CharRenderBuffer {

  def write(line: Int, col: Int, str: String): Unit = {
    write(line, col, str, Nil)
  }

  def write(line: Int, col: Int, str: String, option: Any): Unit

  def asString: String
}

/**
  * TODO : documentation
  */
class ColorRenderBuffer(buffer: ArrayBuffer[ArrayBuffer[String]]) extends CharRenderBuffer {

  override def write(line: Int, col: Int, str: String, option: Any): Unit = {
    val color = option.toString
    val lineBuffer = buffer(line)
    for (i <- 0 until str.length) {
      lineBuffer(col + i) = color + str.charAt(i) + AnsiColors.ANSI_RESET
    }
  }

  override def asString: String = {
    val stringBuilder = new StringBuilder
    buffer foreach (_ foreach (stringBuilder ++= _))
    stringBuilder.result
  }
}

object ColorRenderBuffer {

  def apply(width: Int, height: Int): ColorRenderBuffer = {
    val lineBuffer = createStringBuffer(width, height)
    new ColorRenderBuffer(lineBuffer)
  }

  private def createStringBuffer(width: Int, height: Int): ArrayBuffer[ArrayBuffer[String]] = {
    val buffer = new ArrayBuffer[ArrayBuffer[String]](height)
    for (_ <- 0 until height) {
      val lineBuffer = new ArrayBuffer[String](width + 1)
      buffer += lineBuffer
      for (_ <- 0 until width) {
        lineBuffer += " "
      }
      lineBuffer += s"\n"
    }
    buffer
  }
}
