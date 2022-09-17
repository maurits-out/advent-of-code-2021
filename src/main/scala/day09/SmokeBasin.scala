package day09

import scala.annotation.tailrec
import scala.collection.immutable.Queue
import scala.io.Source
import scala.util.Using

type Height = Int

case class Position(row: Int, column: Int)

case class HeightMap(floor: Vector[Vector[Height]]):
  private val height = floor.size
  private val width = floor(0).size

  def height(pos: Position): Height = floor(pos.row)(pos.column)

  def neighborsOf(pos: Position): List[(Position, Height)] =
    val Position(row, column) = pos
    List(
      Option.when(row > 0)(Position(row - 1, column)),
      Option.when(row < height - 1)(Position(row + 1, column)),
      Option.when(column > 0)(Position(row, column - 1)),
      Option.when(column < width - 1)(Position(row, column + 1))
    ).flatten.map(pos => pos -> height(pos))

  def lowPoints: LazyList[Position] =
    LazyList.range(0, height).flatMap { row =>
      LazyList.range(0, width).map { column =>
        val pos = Position(row, column)
        (height(pos), pos, neighborsOf(pos).map { case (_, height) => height })
      }
    }.collect {
      case (height, pos, heightOfNeighbors) if heightOfNeighbors.forall(height < _) => pos
    }

  def basinSize(lowPoint: Position): Int =
    @tailrec
    def basinSize(visited: Set[Position], toVisit: Queue[Position], acc: Set[Position]): Int =
      if toVisit.isEmpty then
        acc.size
      else
        val (currentPos, remaining) = toVisit.dequeue
        val newPos = neighborsOf(currentPos).collect {
          case (pos, height) if !visited.contains(pos) && height != 9 => pos
        }
        basinSize(visited + currentPos, remaining ++ newPos, acc ++ newPos)

    basinSize(Set.empty, Queue(lowPoint), Set(lowPoint))

end HeightMap

object HeightMap:
  def fromString(raw: String): HeightMap =
    val floor = raw
      .linesIterator
      .map(line => line.map(_.asDigit).toVector)
      .toVector
    HeightMap(floor)
end HeightMap

def readInput(): String =
  Using.resource(getClass.getResourceAsStream("/day09.txt")) {
    stream => Source.fromInputStream(stream).mkString
  }

def part1(heightMap: HeightMap): Unit =
  val answer = heightMap
    .lowPoints
    .map(heightMap.height(_) + 1)
    .sum
  println(s"Part 1: $answer")
end part1

def part2(heightMap: HeightMap): Unit =
  val answer = heightMap
    .lowPoints
    .map(heightMap.basinSize)
    .sorted(Ordering[Int].reverse)
    .take(3)
    .product
  println(s"Part 2: $answer")
end part2

@main
def main(): Unit =
  val heightMap = HeightMap.fromString(readInput())
  part1(heightMap)
  part2(heightMap)
