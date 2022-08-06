package day07

import java.lang.Math.abs
import scala.io.Source
import scala.util.Using

def readInput(): String =
  Using.resource(getClass.getResourceAsStream("/day07.txt")) {
    stream => Source.fromInputStream(stream).mkString.trim
  }

def initialPositions(input: String): List[Int] =
  input.split(",").map(_.toInt).toList

def median(positions: List[Int]): Int =
  val sorted = positions.sorted
  val middle = sorted.size / 2
  if sorted.size % 2 == 1 then
    sorted(middle)
  else
    (sorted(middle - 1) + sorted(middle)) / 2

def fuelPart1(positions: List[Int]): Int =
  val m = median(positions)
  positions.map(p => abs(m - p)).sum

def increasingFuelCosts(position: Int, average: Int): Int =
  val diff = abs(average - position)
  (diff * (diff + 1)) / 2

def average(positions: List[Int]): Int = positions.sum / positions.size

def fuelPart2(positions: List[Int]): Int =
  val avg = average(positions)
  positions.map(increasingFuelCosts(_, avg)).sum

@main
def main(): Unit =
  val positions = initialPositions(readInput())
  println(s"Part 1: ${fuelPart1(positions)}")
  println(s"Part 2: ${fuelPart2(positions)}")
