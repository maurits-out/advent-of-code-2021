package day20

import scala.annotation.tailrec
import scala.io.Source
import scala.util.Using
import scala.util.chaining.*

class TrenchMap(input: String):

  private val (algorithm, inputImage) = parse(input)

  private case class Cell(row: Int, column: Int)

  private def parseAlgorithm(str: String) =
    (for (ch, i) <- str.zipWithIndex
         digit = toBinaryDigit(ch)
    yield i -> digit).toMap

  private def toBinaryDigit(ch: Char) =
    if ch == '.' then 0 else 1

  private def parseInputImage(str: String) =
    (for (line, r) <- str.linesIterator.zipWithIndex
         (ch, c) <- line.zipWithIndex
    yield Cell(r, c) -> toBinaryDigit(ch)).toMap

  private def parse(input: String) =
    val Array(algInput, imageInput) = input.split("\n\n")
    (parseAlgorithm(algInput), parseInputImage(imageInput))

  private def getSquare(cell: Cell) = {
    val Cell(row, column) = cell
    Seq(
      Cell(row - 1, column - 1),
      Cell(row - 1, column),
      Cell(row - 1, column + 1),
      Cell(row, column - 1),
      Cell(row, column),
      Cell(row, column + 1),
      Cell(row + 1, column - 1),
      Cell(row + 1, column),
      Cell(row + 1, column + 1)
    )
  }

  private def getValue(cell: Cell, image: Map[Cell, Int], default: Int) =
    getSquare(cell)
      .map(image.getOrElse(_, default))
      .foldLeft(0) {
        (acc, digit) => (acc * 2) + digit
      }
      .pipe(algorithm)

  private def enhanceImage(image: Map[Cell, Int], default: Int) =
    val rowMin = image.keys.map(_.row).min - 1
    val rowMax = image.keys.map(_.row).max + 1
    val colMin = image.keys.map(_.column).min - 1
    val colMax = image.keys.map(_.column).max + 1
    (for r <- rowMin to rowMax
         c <- colMin to colMax
         cell = Cell(r, c)
         value = getValue(cell, image, default)
    yield cell -> value).toMap

  @tailrec
  private def enhance(img: Map[Cell, Int], times: Int, default: Int = 0): Int =
    if times == 0 then
      img.values.count(_ == 1)
    else
      enhance(enhanceImage(img, default), times - 1, 1 - default)

  def part1(): Int = enhance(inputImage, 2)
  def part2(): Int = enhance(inputImage, 50)

@main
def main(): Unit =
  val input = Using.resource(getClass.getResourceAsStream("/day20.txt")) { stream =>
    Source.fromInputStream(stream).mkString
  }
  val trenchMap = new TrenchMap(input)
  println(s"Part 1: ${trenchMap.part1()}")
  println(s"Part 2: ${trenchMap.part2()}")
