import com.day1._
import scala.io.Source

class Day1Problem1Tests extends munit.FunSuite {
  test("test") {
    val inputs = List(
      "1abc2",
      "pqr3stu8vwx",
      "a1b2c3d4e5f",
      "treb7uchet"
    )
    assertEquals(problem1(inputs), 142)
  }
  test("challenge") {
    val filename = "src/test/scala/day1/input.txt"
    val inputs = Source.fromFile(filename).getLines.toList
    assertEquals(problem1(inputs), 55621)
  }
}
