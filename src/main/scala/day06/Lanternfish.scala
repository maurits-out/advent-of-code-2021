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
  val s = (1 to 8).map(d => (d - 1) -> state(d)).toMap
  s + (6 -> (s(6) + state(0))) + (8 -> state(0))
