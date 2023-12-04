package com.day3

def getNumberAndPos(line: String): Option[(Int, Int, Int)] =
  line
    .find(_.isDigit)
    .map: c1 =>
      val startPos = line.indexOf(c1)
      val endPos = line
        .drop(startPos)
        .find(c => !(c.isDigit))
        .map(line.drop(startPos).indexOf(_) + startPos)
        .getOrElse(line.length)
      (line.take(endPos).drop(startPos).toInt, startPos, endPos)

def identifyNumbers(
    line: String
): List[(Int, Int, Int)] =
  getNumberAndPos(line) match
    case Some(n, start, end) =>
      List((n, start, end)) ++ identifyNumbers(line.drop(end)).map(d =>
        (d._1, end + d._2, end + d._3)
      )
    case None => List.empty

def identifySymbols(line: String): List[Int] =
  line.zipWithIndex.filter(d => !(d._1.isDigit) & d._1 != '.').map(_._2).toList

def identifyGears(line: String): List[Int] =
  line.zipWithIndex.filter(_._1 == '*').map(_._2).toList

def identifyPartNumbers(
    lineBefore: String,
    line: String,
    lineAfter: String
): List[Int] =
  val nums = identifyNumbers(line)
  val symbols =
    identifySymbols(lineBefore) ++ identifySymbols(line) ++ identifySymbols(
      lineAfter
    )

  nums
    .filter((n, startPos, endPos) =>
      symbols.exists(s => s >= startPos - 1 & s <= endPos)
    )
    .map(_._1)

def identifyGearRatios(
    lineBefore: String,
    line: String,
    lineAfter: String
): List[Int] =
  val gears = identifyGears(line)
  val nums =
    identifyNumbers(lineBefore) ++ identifyNumbers(line) ++ identifyNumbers(
      lineAfter
    )
  gears
    .map: g =>
      val nearNums = nums
        .filter((n, startPos, endPos) => g >= startPos - 1 & g <= endPos)
        .map(_._1)

      if (nearNums.length == 2) nearNums.product else 0

def problem1(lines: List[String]): Int =
  val lineLength = lines.head.length
  val emptyLine = List.fill(lineLength)(".").mkString
  ((emptyLine :: lines) :+ emptyLine)
    .sliding(3)
    .map:
      case List(a, b, c) => identifyPartNumbers(a, b, c)
      case _             => List.empty
    .flatten
    .sum

def problem2(lines: List[String]): Int =
  val lineLength = lines.head.length
  val emptyLine = List.fill(lineLength)(".").mkString
  ((emptyLine :: lines) :+ emptyLine)
    .sliding(3)
    .map:
      case List(a, b, c) => identifyGearRatios(a, b, c).sum
      case _             => 0
    .sum
