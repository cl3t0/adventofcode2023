import com.day2._
import scala.io.Source

class Day2Problem2Tests extends munit.FunSuite {
  test("test") {
    val inputs = List(
      "Game 1: 3 blue, 4 red; 1 red, 2 green, 6 blue; 2 green",
      "Game 2: 1 blue, 2 green; 3 green, 4 blue, 1 red; 1 green, 1 blue",
      "Game 3: 8 green, 6 blue, 20 red; 5 blue, 4 red, 13 green; 5 green, 1 red",
      "Game 4: 1 green, 3 red, 6 blue; 3 green, 6 red; 3 green, 15 blue, 14 red",
      "Game 5: 6 red, 1 blue, 3 green; 2 blue, 1 red, 2 green"
    )
    assertEquals(problem2(inputs), 2286)
  }
  test("challenge") {
    val filename = "src/test/scala/day2/input.txt"
    val inputs = Source.fromFile(filename).getLines.toList
    assertEquals(problem2(inputs), 63981)
  }
}
