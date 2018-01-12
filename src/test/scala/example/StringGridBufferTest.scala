package example

import org.scalatest.FunSuite

class StringGridBufferTest extends FunSuite {

  test("renders characters in correct position") {
    val buffer = StringGridBuffer.default(4, 4)
    buffer.write(0, 0, "1")
    buffer.write(1, 1, "2")
    buffer.write(2, 2, "3")
    buffer.write(3, 3, "4")

    assert(buffer.asString() ==
      """1   
        | 2  
        |  3 
        |   4
        |""".stripMargin)
  }

  test("writes string in correct position") {
    val buffer = StringGridBuffer.default(5, 3)
    buffer.write(1, 1, "123")

    assert(buffer.asString() ==
      """     
        | 123 
        |     
        |""".stripMargin)
  }

  test("use custom line separator") {
    val buffer = StringGridBuffer.default(2, 2)
    assert(buffer.asString("|") == "  |  |")
  }
}
