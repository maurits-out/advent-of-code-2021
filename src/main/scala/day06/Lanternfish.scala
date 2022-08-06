package day06

import scala.io.Source
import scala.util.Using

@main
def main(): Unit =
  val initial = initialState(readInput())
  println(s"Part 1: ${simulate(initial, 80)}")
  println(s"Part 2: ${simulate(initial, 256)}")

def readInput(): String =
  Using.resource(getClass.getResourceAsStream("/day06.txt")) { stream =>
    Source.fromInputStream(stream).mkString.trim
  }

def initialState(input: String): Map[Int, Long] = input
  .split(",")
  .groupMapReduce(_.toInt)(_ => 1L)(_ + _)
  .withDefaultValue(0L)

def simulate(initial: Map[Int, Long], days: Int) =
  val end = (1 to days).foldLeft(initial) { (s, _) => nextDay(s) }
  end.values.sum

def nextDay(state: Map[Int, Long]): Map[Int, Long] =
  Map(
    0 -> state(1),
    1 -> state(2),
    2 -> state(3),
    3 -> state(4),
    4 -> state(5),
    5 -> state(6),
    6 -> (state(0) + state(7)),
    7 -> state(8),
    8 -> state(0)
  )
