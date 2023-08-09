package day23

import day23.AmphipodType.destinationColumns

import scala.annotation.tailrec
import scala.collection.mutable

case class Location(row: Int, column: Int) {

  def distanceTo(other: Location): Int = {
    Math.abs(row - other.row) + Math.abs(column - other.column)
  }
}


enum AmphipodType(val energy: Int, val destinationColumn: Int):
  case Amber extends AmphipodType(1, 3)
  case Bronze extends AmphipodType(10, 5)
  case Copper extends AmphipodType(100, 7)
  case Desert extends AmphipodType(1000, 9)

object AmphipodType {
  val destinationColumns: Set[Int] = AmphipodType.values.map(_.destinationColumn).toSet
}

case class Amphipod(location: Location, amphipodType: AmphipodType) {

  def isInHallway: Boolean = {
    location.row == 1
  }
}

case class State(amphipods: Set[Amphipod]) {

  def findAvailableLocationsInHallway(fromColumn: Int): Set[Location] = {

    val occupied = amphipods.filter(_.isInHallway).map(_.location.column)

    @tailrec
    def findAvailableSpotsInHallway(currentColumn: Int, step: Int, spots: Set[Location]): Set[Location] = {
      if currentColumn < 1 || currentColumn > 11 || occupied.contains(currentColumn) then {
        spots
      } else if destinationColumns.contains(currentColumn) then {
        findAvailableSpotsInHallway(currentColumn + step, step, spots)
      } else {
        findAvailableSpotsInHallway(currentColumn + step, step, spots + Location(1, currentColumn))
      }
    }

    val right = findAvailableSpotsInHallway(fromColumn + 1, 1, Set.empty)
    val left = findAvailableSpotsInHallway(fromColumn - 1, -1, Set.empty)

    left union right
  }

  private def findAmphipodInSideRoomThatCanMove(sideRoomColumn: Int): Option[Amphipod] = {
    val amphipodsInSideRoom = amphipods.filter(amphipod => amphipod.location.column == sideRoomColumn)

    if amphipodsInSideRoom.exists(a => a.amphipodType.destinationColumn != sideRoomColumn) then
      Some(amphipodsInSideRoom.minBy(_.location.row))
    else
      None
  }

  private def findAmphipodsThatCanMoveToHallway(): Set[Amphipod] = {
    Set(3, 5, 7, 9).flatMap(findAmphipodInSideRoomThatCanMove)
  }

  private def moveAmphipodToHallway(amphipod: Amphipod): Set[(State, Int)] = {
    findAvailableLocationsInHallway(amphipod.location.column)
      .map(newLocation => {
        val cost = amphipod.amphipodType.energy * amphipod.location.distanceTo(newLocation)
        State(amphipods = amphipods - amphipod + amphipod.copy(location = newLocation)) -> cost
      })
  }

  private def isSideRoomAvailableForAmphipod(amphipod: Amphipod): Boolean = {
    !amphipods.exists { other =>
      other.location.column == amphipod.amphipodType.destinationColumn && other.amphipodType != amphipod.amphipodType
    }
  }

  private def canMoveToSideRoom(amphipod: Amphipod): Boolean = {
    val occupied = amphipods.filter(_.isInHallway).map(_.location.column)
    val start = Math.min(amphipod.location.column, amphipod.amphipodType.destinationColumn) + 1
    val end = Math.max(amphipod.location.column, amphipod.amphipodType.destinationColumn)
    start.until(end).forall(!occupied.contains(_))
  }

  private def findLocationInSideRoom(amphipod: Amphipod): Location = {
    val column = amphipod.amphipodType.destinationColumn
    val row = Seq(2, 3, 4, 5)
      .find(r => !amphipods.exists(a => a.location.row == r && a.location.column == column))
      .get
    Location(row, column)
  }

  private def moveAmphipodToSideRoom(amphipod: Amphipod): (State, Int) = {
    val location = findLocationInSideRoom(amphipod)
    val cost = amphipod.amphipodType.energy * amphipod.location.distanceTo(location)
    State(amphipods = amphipods - amphipod + amphipod.copy(location = location)) -> cost
  }

  def nextStates(): Map[State, Int] = {
    val occupied = amphipods.map(_.location)

    val fromSideRoomToHallway = findAmphipodsThatCanMoveToHallway()
      .flatMap(moveAmphipodToHallway)

    val fromHallwayToSideRoom = amphipods
      .filter(_.isInHallway)
      .filter(isSideRoomAvailableForAmphipod)
      .filter(canMoveToSideRoom)
      .map(moveAmphipodToSideRoom)

    (fromSideRoomToHallway union fromHallwayToSideRoom).toMap
  }

  def isEndState: Boolean = {
    amphipods.forall { case a@Amphipod(location, amphipodType) =>
      !a.isInHallway && location.column == amphipodType.destinationColumn
    }
  }
}

def calculateLeastEnergyToOrganizeAmphipods(startState: State): Int = {

  val dist = mutable.Map(startState -> 0).withDefaultValue(Int.MaxValue)
  val q = mutable.PriorityQueue(startState -> 0)(Ordering.by {
    case (state, energy) => -energy
  })
  val visited = mutable.Set[State]()

  @tailrec
  def dijkstra(): Int = {
    val (state, totalEnergy) = q.dequeue()
    if (state.isEndState) {
      return totalEnergy
    } else if !visited.contains(state) then {
      state.nextStates().foreach {
        case (neighborState, energy) =>
          val s = dist(state) + energy
          if s < dist(neighborState) then {
            dist(neighborState) = s
            q.enqueue(neighborState -> s)
          }
      }
      visited.add(state)
    }
    dijkstra()
  }

  dijkstra()
}


