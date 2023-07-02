package day25

import scala.annotation.tailrec
import scala.io.Source
import scala.util.Using
import scala.util.chaining.*

case class Location(row: Int, column: Int)

case class State(height: Int, width: Int, eastFacing: Set[Location], southFacing: Set[Location])

class SeaCucumber(input: String):

  def part1(): Int =
    @tailrec
    def stepUntilSeaCucumbersStopMoving(state: State, count: Int): Int =
      val nextState = state.pipe(stepEast).pipe(stepSouth)
      if nextState == state then
        count
      else
        stepUntilSeaCucumbersStopMoving(nextState, count + 1)

    stepUntilSeaCucumbersStopMoving(parseInput(), 1)
  end part1

  private def stepEast(state: State): State =
    def moveEastIfAllowed(location: Location): Location =
      val next = Location(
        row = location.row,
        column = if location.column == state.width - 1 then 0 else location.column + 1
      )
      if state.eastFacing.contains(next) || state.southFacing.contains(next) then location else next

    state.copy(eastFacing = state.eastFacing.map(moveEastIfAllowed))
  end stepEast

  private def stepSouth(state: State): State =
    def moveSouthIfAllowed(location: Location): Location =
      val next = Location(
        row = if location.row == state.height - 1 then 0 else location.row + 1,
        column = location.column
      )
      if state.eastFacing.contains(next) || state.southFacing.contains(next) then location else next

    state.copy(southFacing = state.southFacing.map(moveSouthIfAllowed))
  end stepSouth

  private def parseInput(): State =
    val rows = input.linesIterator.toSeq
    State(
      height = rows.length,
      width = rows.head.length,
      eastFacing = extractLocations(rows, '>'),
      southFacing = extractLocations(rows, 'v')
    )
  end parseInput

  private def extractLocations(rows: Seq[String], cucumberType: Char): Set[Location] =
    (for (line, r) <- rows.zipWithIndex
         (ch, c) <- line.zipWithIndex
         if ch == cucumberType
    yield Location(r, c)).toSet
  end extractLocations

end SeaCucumber

@main
def main(): Unit =
  val input = Using.resource(getClass.getResourceAsStream("/day25.txt")) { stream =>
    Source.fromInputStream(stream).mkString
  }
  val seaCucumber = new SeaCucumber(input)
  println(s"Part 1: ${seaCucumber.part1()}")
end main
