import cats.effect.{ExitCode, IO, IOApp}

import scala.io.Source

object Day1 extends IOApp {
  private def getTotals(lines: List[String]): Map[Int, Int] =
    lines.foldLeft(Map[Int, Int]().empty) { case (totals, line) =>
      val highestKey = totals.keys.toList match {
        case Nil => 0
        case l@_ => l.max
      }
      line match {
        case "" => totals.updated(highestKey + 1, 0)
        case n if n.forall(Character.isDigit) => totals.updatedWith(highestKey) {
          case Some(existing) => Some(existing + n.toInt)
          case None => Some(n.toInt)
        }
      }
    }

  private def getHighestTotal(totals: Map[Int, Int]): Option[(Int, Int)] =
    totals
      .toList
      .sortWith { case ((_, firstTotal), (_, secondTotal)) => firstTotal > secondTotal }
      .headOption

  private def getHighestThreeTotals(totals: Map[Int, Int]): List[(Int, Int)] =
    totals
      .toList
      .sortWith { case ((_, firstTotal), (_, secondTotal)) => firstTotal > secondTotal }
      .take(3)

  override def run(args: List[String]): IO[ExitCode] = {
    for {
      lines <- IO(Source.fromResource("day1.txt").getLines().toList)
      totals = getTotals(lines)
      highestTotal = getHighestTotal(totals)
      _ = highestTotal.foreach(t => println(s"highest total: $t"))
      highestThreeTotals = getHighestThreeTotals(totals)
      _ = println(highestThreeTotals)
      _ = println(s"highest 3 totals: ${highestThreeTotals.map(_._2).sum}")
    } yield ExitCode.Success
  }
}
