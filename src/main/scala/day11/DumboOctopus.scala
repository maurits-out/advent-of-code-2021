package day11

import scala.annotation.tailrec
import scala.collection.immutable.Queue
import scala.io.Source
import scala.util.Using

case class EnergyLevel(value: Int) extends AnyVal :
  def increment(): EnergyLevel =
    EnergyLevel(value + 1)

  def shouldFlash(): Boolean =
    value > 9
end EnergyLevel


case class Location(row: Int, column: Int):
  def adjacentLocations(): List[Location] = List(
    copy(row = row - 1, column = column - 1),
    copy(row = row - 1),
    copy(row = row - 1, column = column + 1),
    copy(column = column - 1),
    copy(column = column + 1),
    copy(row = row + 1, column = column - 1),
    copy(row = row + 1),
    copy(row = row + 1, column = column + 1),
  )
end Location


case class Grid(octopuses: Map[Location, EnergyLevel]):
  def step(): (Grid, Int) =
    val afterEnergyLevelIncrease = increaseEnergyLevel(octopuses)
    val (afterFlash, flashCount) = flash(afterEnergyLevelIncrease)
    (Grid(resetEnergyLevel(afterFlash)), flashCount)

  def octopusCount(): Int = octopuses.size

  private def increaseEnergyLevel(current: Map[Location, EnergyLevel]) =
    current.map { (location, level) =>
      (location, level.increment())
    }

  private def flash(current: Map[Location, EnergyLevel]): (Map[Location, EnergyLevel], Int) =

    @tailrec
    def iterate(toFlash: Queue[Location], collected: Set[Location], current: Map[Location, EnergyLevel]): (Map[Location, EnergyLevel], Int) =
      toFlash.dequeueOption match
        case None => (current, collected.size)
        case Some(location, remaining) =>
          val updatedAdjacent = location.adjacentLocations().foldLeft(current) { (s, l) =>
            s.get(l) match
              case Some(level) => s.updated(l, level.increment())
              case None => s
          }
          val toFlash = findOctopusesToFlash(updatedAdjacent, collected)
          iterate(remaining ++ toFlash, collected ++ toFlash, updatedAdjacent)

    val collected = findOctopusesToFlash(current, Set.empty)
    iterate(Queue(collected.toSeq: _*), collected, current)
  end flash

  private def findOctopusesToFlash(octopuses: Map[Location, EnergyLevel], collected: Set[Location]): Set[Location] =
    octopuses.collect {
      case (location, level) if level.shouldFlash() && !collected.contains(location) => location
    }.toSet

  private def resetEnergyLevel(current: Map[Location, EnergyLevel]): Map[Location, EnergyLevel] =
    current.map {
      case (location, level) if level.shouldFlash() => (location, EnergyLevel(0))
      case other => other
    }
end Grid


object Grid:
  def apply(): Grid =
    Using.resource(getClass.getResourceAsStream("/day11.txt")) { stream =>
      val octopuses =
        for (line, row) <- Source.fromInputStream(stream).getLines().zipWithIndex
            (char, column) <- line.zipWithIndex
        yield Location(row, column) -> EnergyLevel(char.asDigit)
      Grid(octopuses.toMap)
    }
end Grid


case class State(grid: Grid, stepCount: Int = 0, flashCount: Int = 0, lastFlash: Int = 0):
  def update(grid: Grid, flashed: Int): State =
    copy(grid = grid, stepCount = stepCount + 1, flashCount = flashCount + flashed, lastFlash = flashed)

trait StopLogic:
  def shouldStop(state: State): Boolean

case class StopAfterNRounds(n: Int) extends StopLogic :
  override def shouldStop(state: State): Boolean =
    state.stepCount == n

case class stopAfterAllFlashed() extends StopLogic :
  override def shouldStop(state: State): Boolean =
    state.lastFlash == state.grid.octopusCount()


class Simulator:

  private val initialState = State(Grid())

  def run(stopCriterion: StopLogic): State =
    @tailrec
    def iterate(state: State): State =
      if stopCriterion.shouldStop(state) then
        state
      else
        val (grid, flashed) = state.grid.step()
        iterate(state.update(grid, flashed))

    iterate(initialState)

end Simulator


def part1(simulator: Simulator): Unit = {
  val answer = simulator.run(StopAfterNRounds(100)).flashCount
  println(s"Part 1: $answer")
}

def part2(simulator: Simulator): Unit = {
  val answer = simulator.run(stopAfterAllFlashed()).stepCount
  println(s"Part 2: $answer")
}

@main
def dumboOctopus(): Unit =
  val simulator = new Simulator()
  part1(simulator)
  part2(simulator)
