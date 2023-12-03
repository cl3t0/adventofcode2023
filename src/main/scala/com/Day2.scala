package com.day2

case class BallN(red: Int = 0, green: Int = 0, blue: Int = 0):
  def +(another: BallN): BallN =
    BallN(red + another.red, green + another.green, blue + another.blue)

  def power: Int =
    red * green * blue

  def max(another: BallN): BallN =
    BallN(
      red.max(another.red),
      green.max(another.green),
      blue.max(another.blue)
    )

  def lessThen(another: BallN): Boolean =
    red <= another.red & green <= another.green & blue <= another.blue

val maxBallN = BallN(12, 13, 14)

def getGameNum(line: String): Int =
  line.take(line.indexOf(":")).drop(5).toInt

def splitRounds(line: String): List[String] =
  line.drop(line.indexOf(":") + 1).split("; ").toList

def splitBalls(round: String): List[String] =
  round.split(", ").toList

def getBallQuantity(ballString: String): BallN =
  ballString match
    case s"$n red"   => BallN(n.strip.toInt, 0, 0)
    case s"$n green" => BallN(0, n.strip.toInt, 0)
    case s"$n blue"  => BallN(0, 0, n.strip.toInt)
    case _           => BallN(0, 0, 0)

def minimumBallQuantity(game: String): BallN =
  splitRounds(game)
    .map(
      splitBalls(_).map(getBallQuantity).fold(BallN())(_ + _)
    )
    .fold(BallN())(_.max(_))

def problem1(lines: List[String]): Int =
  lines
    .filter(minimumBallQuantity(_).lessThen(maxBallN))
    .map(getGameNum)
    .sum

def problem2(lines: List[String]): Int =
  lines.map(minimumBallQuantity(_).power).sum
