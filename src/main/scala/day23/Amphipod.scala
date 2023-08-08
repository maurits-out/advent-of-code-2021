package day23

import day23.AmphipodType.{Amber, Bronze, Copper, Desert}

import scala.annotation.tailrec
import scala.collection.mutable

case class Location(row: Int, column: Int) {

  def distanceTo(other: Location): Int = {
    Math.abs(row - other.row) + Math.abs(column - other.column)
  }
}

enum AmphipodType(val energy: Int):
  case Amber extends AmphipodType(1)
  case Bronze extends AmphipodType(10)
  case Copper extends AmphipodType(100)
  case Desert extends AmphipodType(1000)

case class Amphipod(location: Location, amphipodType: AmphipodType) {

  def heuristicDistanceToDestinationSiteRoom(): Int = {
    if ((location.row >= 2) && location.column == Burrow.SiteRoomColumns(amphipodType)) {
      0
    } else {
      amphipodType.energy * (location.row + Math.abs(Burrow.SiteRoomColumns(amphipodType) - location.column))
    }
  }
}

case class State(amphipods: Set[Amphipod]) {

  def heuristicDistanceToEndState(): Int = {
    amphipods.toSeq.map(_.heuristicDistanceToDestinationSiteRoom()).sum
  }

  def findAvailableLocationsInHallway(from: Location): Set[Location] = {

    val occupied = amphipods.map(_.location)

    @tailrec
    def findAvailableSpotsInHallway(current: Location, step: Int, spots: Set[Location]): Set[Location] = {
      if !Burrow.AllSpaces.contains(current) || occupied.contains(current) then {
        spots
      } else if Burrow.Forbidden.contains(current) then {
        findAvailableSpotsInHallway(current.copy(column = current.column + step), step, spots)
      } else {
        findAvailableSpotsInHallway(current.copy(column = current.column + step), step, spots + current)
      }
    }

    val right = findAvailableSpotsInHallway(from.copy(column = from.column + 1), 1, Set.empty)
    val left = findAvailableSpotsInHallway(from.copy(column = from.column - 1), -1, Set.empty)

    left union right
  }

  def nextStates(): Map[State, Int] = {
    val occupied = amphipods.map(_.location)

    val fromSideRoomToHallway = amphipods
      .filter { a =>
        a.location.row == 2 || (a.location.row == 3 && !occupied.contains(Location(2, a.location.column))) &&
          a.location.column != Burrow.SiteRoomColumns(a.amphipodType)
      }
      .flatMap { a =>
        val availableLocationsInHallway = findAvailableLocationsInHallway(Location(1, a.location.column))
        availableLocationsInHallway.map(newLocation => {
          val cost = a.amphipodType.energy * a.location.distanceTo(newLocation)
          State(amphipods = amphipods - a + a.copy(location = newLocation)) -> cost
        })
      }.toMap

    val fromHallwayToSideRoom = amphipods
      .filter(_.location.row == 1)
      .filter {
        a => {
          Burrow.SiteRooms(a.amphipodType).forall { location =>
            !amphipods.exists { other =>
              other.location == location && other.amphipodType != a.amphipodType
            }
          }
        }
      }
      .filter { a =>
        val destColumn = Burrow.SiteRoomColumns(a.amphipodType)
        val rowFree = if (a.location.column < destColumn) {
          (a.location.column + 1).until(destColumn).forall(c => !occupied.contains(Location(1, c)))
        } else {
          (destColumn + 1).until(a.location.column).forall(c => !occupied.contains(Location(1, c)))
        }
        rowFree && !occupied.contains(Location(2, destColumn))
      }
      .map {
        a => {
          val location = Location(3, Burrow.SiteRoomColumns(a.amphipodType))
          val newLocation = if (occupied.contains(location)) {
            Location(2, Burrow.SiteRoomColumns(a.amphipodType))
          } else {
            location
          }
          val cost = a.amphipodType.energy * a.location.distanceTo(newLocation)
          State(amphipods = amphipods - a + a.copy(location = newLocation)) -> cost
        }
      }.toMap

    fromSideRoomToHallway ++ fromHallwayToSideRoom
  }

  def isEndState: Boolean = {
    amphipods.forall { case Amphipod(location, amphipodType) =>
      (location.row == 2 || location.row == 3) &&
        location.column == Burrow.SiteRoomColumns(amphipodType)
    }
  }
}

object Burrow {
  val SiteRooms: Map[AmphipodType, Set[Location]] = Map(
    Amber -> Set(Location(2, 3), Location(3, 3)),
    Bronze -> Set(Location(2, 5), Location(3, 5)),
    Copper -> Set(Location(2, 7), Location(3, 7)),
    Desert -> Set(Location(2, 9), Location(3, 9)),
  )

  val SiteRoomColumns: Map[AmphipodType, Int] = Map(
    Amber -> 3,
    Bronze -> 5,
    Copper -> 7,
    Desert -> 9,
  )

  val AllSpaces: Set[Location] = (1 to 11).map(Location(1, _)).toSet union SiteRooms.values.reduce(_ union _)

  val Forbidden: Set[Location] = Set(Location(1, 3), Location(1, 5), Location(1, 7), Location(1, 9))
}

def calculateLeastEnergyToOrganizeAmphipods(startState: State): Int = {

  val fScore: mutable.Map[State, Int] = mutable.HashMap()
  val gScore: mutable.Map[State, Int] = mutable.HashMap().withDefaultValue(Int.MaxValue)
  val open: mutable.PriorityQueue[State] = mutable.PriorityQueue()(Ordering.by(s => -fScore(s)))

  @tailrec
  def aStar(): Int = {
    if (open.isEmpty) {
      throw new IllegalStateException("End state was not reached")
    }
    val current = open.dequeue()
    if (current.isEndState) {
      gScore(current)
    } else {
      val neighborToTentativeGScore = current.nextStates().view.mapValues(_ + gScore(current)).filter {
        (neighbor, tentativeGScore) => tentativeGScore < gScore(neighbor)
      }.toMap

      gScore.addAll(neighborToTentativeGScore)
      fScore.addAll(neighborToTentativeGScore.map {
        case (neighbor, tentativeGScore) =>
          neighbor -> (tentativeGScore + fScore.getOrElse(neighbor, 0))
      })
      open.enqueue(neighborToTentativeGScore.keySet.toSeq: _*)

      aStar()
    }
  }


  fScore(startState) = startState.heuristicDistanceToEndState()
  gScore(startState) = 0
  open.enqueue(startState)

  aStar()
}


