package day13

import scala.io.Source
import scala.util.Using

case class Dot(x: Int, y: Int)

case class Instruction(axis: Char, lines: Int)

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
      case s"fold along $a=$l" => Instruction(a(0), l.toInt)
    }
    .toList

def parseInput(input: String): (Set[Dot], List[Instruction]) =
  val Array(dots, instructions) = input.split("\n\n")
  (parseDots(dots), parseInstructions(instructions))

def foldLeft(dots: Set[Dot], line: Int): Set[Dot] =
  dots.map {
    case Dot(x, y) if x > line => Dot(x - 2 * (x - line), y)
    case dot => dot
  }

def foldUp(dots: Set[Dot], line: Int): Set[Dot] =
  dots.map {
    case Dot(x, y) if y > line => Dot(x, y - 2 * (y - line))
    case dot => dot
  }

def applyInstruction(instruction: Instruction, dots: Set[Dot]): Set[Dot] =
  val Instruction(axis, line) = instruction
  axis match
    case 'x' => foldLeft(dots, line)
    case 'y' => foldUp(dots, line)

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
