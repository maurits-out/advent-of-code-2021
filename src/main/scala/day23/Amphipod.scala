package day23

import day23.AmphipodType.destinationColumns

import scala.annotation.tailrec
import scala.collection.mutable

case class Location(row: Int, column: Int):
  def distanceTo(other: Location): Int = Math.abs(row - other.row) + Math.abs(column - other.column)
end Location


enum AmphipodType(val energy: Int, val destinationColumn: Int):
  case Amber extends AmphipodType(1, 3)
  case Bronze extends AmphipodType(10, 5)
  case Copper extends AmphipodType(100, 7)
  case Desert extends AmphipodType(1000, 9)
end AmphipodType


object AmphipodType:
  val destinationColumns: Set[Int] = AmphipodType.values.map(_.destinationColumn).toSet
end AmphipodType


case class Amphipod(location: Location, `type`: AmphipodType):
  def isInHallway: Boolean = location.row == 1
end Amphipod


case class State(amphipods: Set[Amphipod]):

  def findAvailableLocationsInHallway(fromColumn: Int): Set[Location] =

    val occupied = amphipods.filter(_.isInHallway).map(_.location.column)

    @tailrec
    def findAvailableSpotsInHallway(currentColumn: Int, step: Int, spots: Set[Location]): Set[Location] =
      if currentColumn < 1 || currentColumn > 11 || occupied.contains(currentColumn) then
        spots
      else if destinationColumns.contains(currentColumn) then
        findAvailableSpotsInHallway(currentColumn + step, step, spots)
      else
        findAvailableSpotsInHallway(currentColumn + step, step, spots + Location(1, currentColumn))
    end findAvailableSpotsInHallway

    val right = findAvailableSpotsInHallway(fromColumn + 1, 1, Set.empty)
    val left = findAvailableSpotsInHallway(fromColumn - 1, -1, Set.empty)

    left union right
  end findAvailableLocationsInHallway

  private def findAmphipodInSideRoomThatCanMove(sideRoomColumn: Int): Option[Amphipod] =
    val amphipodsInSideRoom = amphipods.filter(amphipod => amphipod.location.column == sideRoomColumn)

    if amphipodsInSideRoom.exists(a => a.`type`.destinationColumn != sideRoomColumn) then
      Some(amphipodsInSideRoom.minBy(_.location.row))
    else
      None
  end findAmphipodInSideRoomThatCanMove

  private def findAmphipodsThatCanMoveToHallway(): Set[Amphipod] = destinationColumns.flatMap(findAmphipodInSideRoomThatCanMove)

  private def moveAmphipodToHallway(amphipod: Amphipod): Set[(State, Int)] =
    findAvailableLocationsInHallway(amphipod.location.column)
      .map(newLocation => {
        val cost = amphipod.`type`.energy * amphipod.location.distanceTo(newLocation)
        State(amphipods = amphipods - amphipod + amphipod.copy(location = newLocation)) -> cost
      })
  end moveAmphipodToHallway

  private def isSideRoomAvailableForAmphipod(amphipod: Amphipod): Boolean =
    !amphipods.exists { other =>
      other.location.column == amphipod.`type`.destinationColumn && other.`type` != amphipod.`type`
    }
  end isSideRoomAvailableForAmphipod

  private def canMoveToSideRoom(amphipod: Amphipod): Boolean =
    val occupied = amphipods.filter(_.isInHallway).map(_.location.column)
    val start = Math.min(amphipod.location.column, amphipod.`type`.destinationColumn) + 1
    val end = Math.max(amphipod.location.column, amphipod.`type`.destinationColumn)
    start.until(end).forall(!occupied.contains(_))
  end canMoveToSideRoom

  private def findLocationInSideRoom(amphipod: Amphipod): Location =
    val column = amphipod.`type`.destinationColumn
    val row = Seq(2, 3, 4, 5)
      .find(r => !amphipods.exists(a => a.location.row == r && a.location.column == column))
      .get
    Location(row, column)
  end findLocationInSideRoom

  private def moveAmphipodToSideRoom(amphipod: Amphipod): (State, Int) =
    val location = findLocationInSideRoom(amphipod)
    val cost = amphipod.`type`.energy * amphipod.location.distanceTo(location)
    State(amphipods = amphipods - amphipod + amphipod.copy(location = location)) -> cost
  end moveAmphipodToSideRoom

  def nextStates(): Set[(State, Int)] =
    val fromSideRoomToHallway = findAmphipodsThatCanMoveToHallway()
      .flatMap(moveAmphipodToHallway)

    val fromHallwayToSideRoom = amphipods
      .filter(_.isInHallway)
      .filter(isSideRoomAvailableForAmphipod)
      .filter(canMoveToSideRoom)
      .map(moveAmphipodToSideRoom)

    fromSideRoomToHallway union fromHallwayToSideRoom
  end nextStates

  def isEndState: Boolean =
    amphipods.forall(a => !a.isInHallway && a.location.column == a.`type`.destinationColumn)
  end isEndState
end State


def calculateLeastEnergy(startState: State): Int =
  val dist = mutable.Map(startState -> 0).withDefaultValue(Int.MaxValue)
  val q = mutable.PriorityQueue(startState -> 0)(Ordering.by((_, energy) => -energy))
  val visited = mutable.Set[State]()

  while (q.nonEmpty)
    val (state, totalEnergy) = q.dequeue()
    if (state.isEndState)
      return totalEnergy
    end if
    if !visited.contains(state) then
      state.nextStates().foreach { (neighborState, energy) =>
        val d = dist(state) + energy
        if d < dist(neighborState) then
          dist(neighborState) = d
          q.enqueue(neighborState -> d)
        end if
      }
      visited.add(state)
    end if
  end while
    throw new IllegalStateException("end state not found")
end calculateLeastEnergy
