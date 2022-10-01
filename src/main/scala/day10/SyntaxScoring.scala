package day10

import scala.annotation.tailrec
import scala.io.Source
import scala.util.Using

abstract class LineType

case class Corrupt(illegalChar: Char) extends LineType :
  def score: Int =
    illegalChar match
      case ')' => 3
      case ']' => 57
      case '}' => 1197
      case '>' => 25137

case class Incomplete(closingChars: List[Char]) extends LineType :
  def score: Long =
    val charScore = Map(')' -> 1, ']' -> 2, '}' -> 3, '>' -> 4)
    closingChars.foldLeft(0L) {
      (acc, ch) => (acc * 5) + charScore(ch)
    }

def toLineTypes(lines: List[String]): List[LineType] =
  val openToClose = Map('(' -> ')', '[' -> ']', '{' -> '}', '<' -> '>')

  @tailrec
  def lineType(remaining: List[Char], expected: List[Char]): LineType =
    remaining match
      case Nil => Incomplete(expected)
      case ch :: cs if openToClose.contains(ch) => lineType(cs, openToClose(ch) :: expected)
      case ch :: cs if ch == expected.head => lineType(cs, expected.tail)
      case ch :: _ => Corrupt(ch)

  for {line <- lines} yield lineType(line.toList, Nil)

def readInput(): List[String] =
  Using.resource(getClass.getResourceAsStream("/day10.txt")) {
    stream => Source.fromInputStream(stream).getLines().toList
  }

def part1(lineTypes: List[LineType]): Unit =
  val answer = lineTypes.collect { case c: Corrupt => c.score }.sum
  println(s"Part 1: $answer")

def part2(lineTypes: List[LineType]): Unit =
  val scores = lineTypes.collect { case i: Incomplete => i.score }.toVector.sorted
  val answer = scores(scores.length / 2)
  println(s"Part 2: $answer")

@main
def syntaxScoring(): Unit =
  val lineTypes = toLineTypes(readInput())
  part1(lineTypes)
  part2(lineTypes)
