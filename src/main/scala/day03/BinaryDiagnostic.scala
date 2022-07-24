package day03

import scala.io.Source
import scala.util.Using

object BinaryDiagnostic {

  private def readReport(): List[String] =
    Using.resource(getClass.getResourceAsStream("/day03.txt")) { stream =>
      Source.fromInputStream(stream).getLines().toList
    }

  private def part1(report: List[String]): Unit = {
    val consumption = new PowerConsumption(report)
    val answer = consumption.calculate()
    println(s"Part 1: $answer")
  }

  private def part2(report: List[String]): Unit = {
    val rating = new LifeSupportRating(report)
    val answer = rating.calculate()
    println(s"Part 2: $answer")
  }

  def main(args: Array[String]): Unit = {
    val report = readReport()
    part1(report)
    part2(report)
  }
}
