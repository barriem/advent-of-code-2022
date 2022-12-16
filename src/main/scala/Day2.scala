import cats.effect.{ExitCode, IO, IOApp}
import cats.syntax.traverse.*

import scala.io.Source

object Day2 extends IOApp {
  enum Hand {
    case Rock
    case Paper
    case Scissors
  }

  enum Outcome {
    case Win
    case Lose
    case Draw
  }

  case class Game(myHand: Hand, theirHand: Hand)
  case class GameResult(game: Game, outcome: Outcome)

  import Hand.*
  import Outcome.*

  private val theirHandMappings = Map[String, Hand](
  "A" -> Rock,
  "B" -> Paper,
  "C" -> Scissors
  )

  private val myHandMappings = Map[String, Hand](
    "X" -> Rock,
    "Y" -> Paper,
    "Z" -> Scissors
  )

  private val handScores = Map[Hand, Int](
    Rock -> 1,
    Paper -> 2,
    Scissors -> 3
  )

  private val outcomeScores = Map[Outcome, Int](
    Win -> 6,
    Draw -> 3,
    Lose -> 0
  )

  private val myHandOutcomeMappings = Map[String, Outcome](
    "X" -> Lose,
    "Y" -> Draw,
    "Z" -> Win
  )

  private def getOutcome(game: Game): GameResult =
    (game.myHand, game.theirHand) match {
      case (Rock, Scissors) | (Scissors, Paper) | (Paper, Rock) => GameResult(game, Win)
      case (Rock, Rock) | (Scissors, Scissors) | (Paper, Paper) => GameResult(game, Draw)
      case (Rock, Paper) | (Scissors, Rock) | (Paper, Scissors) => GameResult(game, Lose)
    }

  private def getHandFromOutcome(theirHand: Hand, desiredOutcome: Outcome): Hand =
    (theirHand, desiredOutcome) match {
      case (Rock, Win) => Paper
      case (Paper, Win) => Scissors
      case (Scissors, Win) => Rock
      case (Rock, Draw) => Rock
      case (Paper, Draw) => Paper
      case (Scissors, Draw) => Scissors
      case (Rock, Lose) => Scissors
      case (Paper, Lose) => Rock
      case (Scissors, Lose) => Paper
    }

  private def parseLinePart1(line: String): IO[Game] =
    for {
      (firstChar, lastChar) <- IO(line.charAt(0).toString -> line.charAt(2).toString)
      theirHand <- IO.fromOption(theirHandMappings.get(firstChar))(new RuntimeException(s"cannot find mapping for $firstChar"))
      myHand <- IO.fromOption(myHandMappings.get(lastChar))(new RuntimeException(s"cannot find mapping for $lastChar"))
    } yield Game(myHand, theirHand)

  private def parseLinePart2(line: String): IO[Game] =
    for {
      (firstChar, lastChar) <- IO(line.charAt(0).toString -> line.charAt(2).toString)
      theirHand <- IO.fromOption(theirHandMappings.get(firstChar))(new RuntimeException(s"cannot find mapping for $firstChar"))
      desiredOutcome <- IO.fromOption(myHandOutcomeMappings.get(lastChar))(new RuntimeException(s"cannot find mapping for $lastChar"))
      myHand = getHandFromOutcome(theirHand, desiredOutcome)
    } yield Game(myHand, theirHand)

  private def calculateScore(gameResult: GameResult): Int =
    handScores(gameResult.game.myHand) + outcomeScores(gameResult.outcome)

  private def part1(): IO[Unit] =
    for {
      lines <- IO(Source.fromResource("day2.txt").getLines().toList)
      games <- lines.map(parseLinePart1).sequence
      gameResults = games.map(getOutcome)
      scores = gameResults.map(calculateScore)
      totalScore = scores.sum
      _ = println(s"Part 1 - total score: $totalScore")
    } yield ()

  private def part2(): IO[Unit] =
    for {
      lines <- IO(Source.fromResource("day2.txt").getLines().toList)
      games <- lines.map(parseLinePart2).sequence
      gameResults = games.map(getOutcome)
      scores = gameResults.map(calculateScore)
      totalScore = scores.sum
      _ = println(s"Part 2 - total score: $totalScore")
    } yield ()

  override def run(args: List[String]): IO[ExitCode] = {
    for {
      _ <- part1()
      _ <- part2()
    } yield ExitCode.Success
  }
}
