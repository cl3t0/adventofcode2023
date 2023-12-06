package com.day4

def getCardNumAndContent(line: String): (Int, String) =
  val segPos = line.indexOf(":")
  (line.take(segPos).drop(4).strip.toInt, line.drop(segPos + 1))

def splitWinningAndOwnedNumbers(line: String): (List[Int], List[Int]) =
  line.replace("  ", " ").split(" \\| ").toList match
    case List(a, b) =>
      (
        a.strip.split(" ").toList.map(_.strip.toInt),
        b.strip.split(" ").toList.map(_.strip.toInt)
      )
    case x =>
      println("Wrong '|' quantity")
      println(x)
      (List.empty, List.empty)

def problem1(lines: List[String]): Int =
  lines
    .map: line =>
      val (card, content) = getCardNumAndContent(line)
      val (winning, owned) = splitWinningAndOwnedNumbers(content)
      val winningQuantity = owned.count(winning.contains)
      if (winningQuantity == 0) 0
      else scala.math.pow(2, winningQuantity - 1).toInt
    .sum

def problem2(lines: List[String]): Int =
  lines
    .foldLeft(Range(1, lines.length + 1).map(k => k -> 1).toMap): (acc, line) =>
      val (card, content) = getCardNumAndContent(line)
      val (winning, owned) = splitWinningAndOwnedNumbers(content)
      val winningQuantity = owned.count(winning.contains)
      val newKeys =
        Range(card + 1, card + 1 + winningQuantity)
          .map(k => k -> (acc(k) + acc(card)))
          .toMap
      acc ++ newKeys
    .values
    .sum
