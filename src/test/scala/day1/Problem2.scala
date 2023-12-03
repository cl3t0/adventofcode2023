import com.day1._
import scala.io.Source

class Day1Problem2Tests extends munit.FunSuite {
  test("test") {
    val inputs = List(
      "two1nine",
      "eightwothree",
      "abcone2threexyz",
      "xtwone3four",
      "4nineeightseven2",
      "zoneight234",
      "7pqrstsixteen"
    )
    assertEquals(problem2(inputs), 281)
  }
  test("challenge") {
    val filename = "src/test/scala/day1/input.txt"
    val inputs = Source.fromFile(filename).getLines.toList
    assertEquals(problem2(inputs), 53592)
  }
}
