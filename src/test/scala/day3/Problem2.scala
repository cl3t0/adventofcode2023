import com.day3._
import scala.io.Source

class Day3Problem2Tests extends munit.FunSuite {
  test("test") {
    val inputs = List(
      "467..114..",
      "...*......",
      "..35..633.",
      "......#...",
      "617*......",
      ".....+.58.",
      "..592.....",
      "......755.",
      "...$.*....",
      ".664.598.."
    )
    assertEquals(problem2(inputs), 467835)
  }
  test("challenge") {
    val filename = "src/test/scala/day3/input.txt"
    val inputs = Source.fromFile(filename).getLines.toList
    assertEquals(problem2(inputs), 82824352)
  }
}
