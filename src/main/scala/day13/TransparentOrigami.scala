package day13

import day13.Instruction.{Horizontal, Vertical}

import scala.io.Source
import scala.util.Using

case class Dot(x: Int, y: Int)

enum Instruction:
  case Horizontal(lines: Int)
  case Vertical(lines: Int)

def readInput(): String =
  Using.resource(getClass.getResourceAsStream("/day13.txt")) { stream =>
    Source.fromInputStream(stream).mkString
  }

def parseDots(section: String): Set[Dot] =
  section
    .linesIterator
    .map {
      case s"$x,$y" => Dot(x.toInt, y.toInt)
    }
    .toSet

def parseInstructions(section: String): List[Instruction] =
  section
    .linesIterator
    .map {
      case s"fold along x=$l" => Vertical(l.toInt)
      case s"fold along y=$l" => Horizontal(l.toInt)
    }
    .toList

def parseInput(input: String): (Set[Dot], List[Instruction]) =
  val Array(dots, instructions) = input.split("\n\n")
  (parseDots(dots), parseInstructions(instructions))

def foldDot(d: Int, line: Int): Int =
  if d > line then line - (d - line) else d

def foldLeft(dots: Set[Dot], line: Int): Set[Dot] =
  for dot <- dots yield dot.copy(x = foldDot(dot.x, line))

def foldUp(dots: Set[Dot], line: Int): Set[Dot] =
  for dot <- dots yield dot.copy(y = foldDot(dot.y, line))

def applyInstruction(instruction: Instruction, dots: Set[Dot]): Set[Dot] =
  instruction match
    case Vertical(line) => foldLeft(dots, line)
    case Horizontal(line) => foldUp(dots, line)

def applyInstructions(instructions: List[Instruction], dots: Set[Dot]) =
  instructions.foldLeft(dots) {
    (dot, instruction) => applyInstruction(instruction, dot)
  }

def format(dots: Set[Dot]): List[String] =
  val (maxX, maxY) = (dots.map(_.x).max, dots.map(_.y).max)
  (0 to maxY).map(y =>
    (0 to maxX).map(x =>
      if dots.contains(Dot(x, y)) then '*' else ' ').mkString)
    .toList

def part1(dots: Set[Dot], instructions: List[Instruction]): Unit =
  val answer = applyInstruction(instructions.head, dots).size
  println(s"Part 1: $answer")

def part2(dots: Set[Dot], instructions: List[Instruction]): Unit =
  val result = applyInstructions(instructions, dots)
  println("Part 2:")
  for line <- format(result)
    do println(line)

@main
def main(): Unit =
  val (dots, instructions) = parseInput(readInput())
  part1(dots, instructions)
  part2(dots, instructions)
