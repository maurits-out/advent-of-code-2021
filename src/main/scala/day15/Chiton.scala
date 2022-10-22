package day15

import scala.annotation.tailrec
import scala.io.Source
import scala.util.Using

case class Location(row: Int, column: Int)

trait Cave:
  val dimension: Int

  def getRiskLevel(location: Location): Int

class NormalCave(riskLevels: Map[Location, Int]) extends Cave :
  override val dimension: Int = riskLevels.keySet.map(_.row).max + 1

  override def getRiskLevel(location: Location): Int = riskLevels(location)

class ExtendedCave(riskLevels: Map[Location, Int]) extends Cave :
  private val originalDimension: Int = riskLevels.keySet.map(_.row).max + 1
  override val dimension: Int = 5 * originalDimension

  override def getRiskLevel(location: Location): Int =
    val Location(row, column) = location
    val riskLevel = riskLevels(Location(row % originalDimension, column % originalDimension))
    val tileHorizontal = column / originalDimension
    val tileVertical = row / originalDimension
    ((riskLevel + tileHorizontal + tileVertical - 1) % 9) + 1

class Chiton(input: String):

  private val riskLevels: Map[Location, Int] = parseInput()
  private val startPos: Location = Location(0, 0)

  def part1(): Int = dijkstra(NormalCave(riskLevels))

  def part2(): Int = dijkstra(ExtendedCave(riskLevels))

  private def dijkstra(cave: Cave): Int =

    @tailrec
    def iterate(visited: Set[Location], currentLocations: Set[Location], distances: Map[Location, Int]): Int =
      if currentLocations.isEmpty then
        distances(Location(cave.dimension - 1, cave.dimension - 1))
      else
        val location = findLocationWithLowestDistance(currentLocations, distances)
        val updatedVisited = visited + location
        val neighbors = findNeighbors(location, updatedVisited, cave.dimension)
        val updatedDistances = neighbors.foldLeft(distances) {
          (d, l) => d + (l -> d.getOrElse(l, Int.MaxValue).min(d(location) + cave.getRiskLevel(l)))
        }
        val updatedLocations = (currentLocations - location).union(neighbors)
        iterate(updatedVisited, updatedLocations, updatedDistances)

    val locations = findNeighbors(startPos, Set.empty, cave.dimension)
    val distances = locations.foldLeft(Map(startPos -> 0)) { (d, l) => d + (l -> cave.getRiskLevel(l)) }
    iterate(Set(startPos), locations, distances)
  end dijkstra

  private def parseInput() =
    val riskLevels =
      for (line, row) <- input.linesIterator.zipWithIndex
          (char, column) <- line.zipWithIndex
      yield Location(row, column) -> char.asDigit
    riskLevels.toMap

  private def findNeighbors(location: Location, visited: Set[Location], dimension: Int) =
    val Location(row, column) = location
    List(
      Option.when(row > 0)(Location(row - 1, column)),
      Option.when(row < dimension - 1)(Location(row + 1, column)),
      Option.when(column > 0)(Location(row, column - 1)),
      Option.when(column < dimension - 1)(Location(row, column + 1)),
    ).flatten.filterNot(visited.contains).toSet

  private def findLocationWithLowestDistance(locations: Set[Location], distances: Map[Location, Int]) =
    locations.minBy(l => distances.getOrElse(l, Int.MaxValue))

end Chiton

@main
def main(): Unit =
  val input = Using.resource(getClass.getResourceAsStream("/day15.txt")) { stream =>
    Source.fromInputStream(stream).mkString
  }
  val chiton = new Chiton(input)
  println(s"Part 1: ${chiton.part1()}")
  println(s"Part 2: ${chiton.part2()}")
