package com.day1

val spelledDigits = Map(
  1 -> "one",
  2 -> "two",
  3 -> "three",
  4 -> "four",
  5 -> "five",
  6 -> "six",
  7 -> "seven",
  8 -> "eight",
  9 -> "nine"
)

case class Digit(n: Int, pos: Int):
  def increasePos =
    Digit(n, pos + 1)

def getFirstSpelledDigit(
    line: String
): Option[Digit] =
  line match
    case "" => None
    case str =>
      spelledDigits
        .find(kv => line.startsWith(kv._2))
        .map(kv => Digit(kv._1, 0))
        .orElse(
          getFirstSpelledDigit(line.drop(1)).map(_.increasePos)
        )

def getLastSpelledDigit(
    line: String
): Option[Digit] =
  line match
    case "" => None
    case str =>
      spelledDigits
        .find(kv => line.endsWith(kv._2))
        .map(kv => Digit(kv._1, line.length - kv._2.length))
        .orElse(getLastSpelledDigit(line.dropRight(1)))

def getFirstDigit(line: String): Option[Digit] =
  line
    .find(_.isDigit)
    .map: d =>
      val firstDigit = d.toString.toInt
      val pos = line.indexOf(firstDigit.toString)
      Digit(firstDigit, pos)

def getLastDigit(line: String): Option[Digit] =
  line
    .findLast(_.isDigit)
    .map: d =>
      val lastDigit = d.toString.toInt
      val pos = line.lastIndexOf(lastDigit.toString)
      Digit(lastDigit, pos)

def problem1(lines: List[String]): Int =
  lines
    .map: line =>
      val firstDigit = getFirstDigit(line)
      val lastDigit = getLastDigit(line)
      (firstDigit.get.n.toString + lastDigit.get.n.toString).toInt
    .sum

def problem2(lines: List[String]): Int =
  lines
    .map: line =>
      val digits = List(
        getFirstDigit(line),
        getLastDigit(line),
        getFirstSpelledDigit(line),
        getLastSpelledDigit(line)
      ).flatten.sortBy(_.pos)
      val firstDigit = digits.head.n
      val lastDigit = digits.last.n
      (firstDigit.toString + lastDigit.toString).toInt
    .sum
