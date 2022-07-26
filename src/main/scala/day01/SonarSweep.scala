package day01

import scala.io.Source
import scala.util.Using

def readMeasurements(): List[Int] =
  Using.resource(getClass.getResourceAsStream("/day01.txt")) { stream =>
    Source.fromInputStream(stream).getLines().map(_.toInt).toList
  }

@main
def part1(): Unit =
  val answer = readMeasurements()
    .sliding(2)
    .count { case Seq(a, b) => a < b }
  println(s"Part 1: $answer")

@main
def part2(): Unit =
  val answer = readMeasurements()
    .sliding(3)
    .map(_.sum)
    .sliding(2)
    .count { case Seq(a, b) => a < b }
  println(s"Part 2: $answer")
