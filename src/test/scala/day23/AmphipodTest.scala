package day23

import day23.AmphipodType.{Amber, Bronze, Copper, Desert}
import org.specs2.mutable.Specification
import org.specs2.specification.core.Fragment

class AmphipodTest extends Specification {

  "State.isEndState" should {

    "return true for the end state" in {
      val state = State(
        amphipods = Set(
          Amphipod(Location(2, 3), Amber),
          Amphipod(Location(3, 3), Amber),
          Amphipod(Location(2, 5), Bronze),
          Amphipod(Location(3, 5), Bronze),
          Amphipod(Location(2, 7), Copper),
          Amphipod(Location(3, 7), Copper),
          Amphipod(Location(2, 9), Desert),
          Amphipod(Location(3, 9), Desert),
        )
      )

      state.isEndState must beTrue
    }

    "return false for the start state in example" in {
      val state = State(
        amphipods = Set(
          Amphipod(Location(2, 3), Bronze),
          Amphipod(Location(3, 3), Amber),
          Amphipod(Location(2, 5), Copper),
          Amphipod(Location(3, 5), Desert),
          Amphipod(Location(2, 7), Bronze),
          Amphipod(Location(3, 7), Copper),
          Amphipod(Location(2, 9), Desert),
          Amphipod(Location(3, 9), Amber),
        )
      )

      state.isEndState must beFalse
    }
  }

  "State.findAvailableLocationsInHallway" should {

    "return all spaces except spaces above side room" in {
      val state = State(amphipods = Set())

      state.findAvailableLocationsInHallway(6) must beEqualTo(Set(
        Location(1, 1),
        Location(1, 2),
        Location(1, 4),
        Location(1, 8),
        Location(1, 10),
        Location(1, 11)
      ))
    }

    "return spaces except spaces above side room until occupied spacer is found" in {
      val state = State(amphipods = Set(
        Amphipod(Location(1, 4), Amber),
        Amphipod(Location(1, 10), Amber)
      ))

      state.findAvailableLocationsInHallway(6) must beEqualTo(Set(
        Location(1, 8)
      ))
    }
  }

  "State.nextStates" should {

    "return all states in hallway for amphipod in first row of side room" in {
      val state = State(amphipods = Set(
        Amphipod(Location(2, 7), Amber)
      ))

      val nextStates = state.nextStates()

      nextStates must beEqualTo(Map(
        State(Set(Amphipod(Location(1, 4), Amber))) -> 4,
        State(Set(Amphipod(Location(1, 8), Amber))) -> 2,
        State(Set(Amphipod(Location(1, 1), Amber))) -> 7,
        State(Set(Amphipod(Location(1, 10), Amber))) -> 4,
        State(Set(Amphipod(Location(1, 6), Amber))) -> 2,
        State(Set(Amphipod(Location(1, 11), Amber))) -> 5,
        State(Set(Amphipod(Location(1, 2), Amber))) -> 6
      ))
    }

    "return all states in hallway for amphipod in second row of side room" in {
      val state = State(amphipods = Set(
        Amphipod(Location(3, 7), Amber)
      ))

      val nextStates = state.nextStates()

      nextStates must beEqualTo(Map(
        State(Set(Amphipod(Location(1, 4), Amber))) -> 5,
        State(Set(Amphipod(Location(1, 8), Amber))) -> 3,
        State(Set(Amphipod(Location(1, 1), Amber))) -> 8,
        State(Set(Amphipod(Location(1, 10), Amber))) -> 5,
        State(Set(Amphipod(Location(1, 6), Amber))) -> 3,
        State(Set(Amphipod(Location(1, 11), Amber))) -> 6,
        State(Set(Amphipod(Location(1, 2), Amber))) -> 7
      ))
    }


    "not return states for amphipod that is blocked in side room" in {
      val state = State(amphipods = Set(
        Amphipod(Location(2, 7), Amber),
        Amphipod(Location(3, 7), Amber)
      ))

      val nextStates = state.nextStates()

      nextStates must beEqualTo(Map(
        State(Set(Amphipod(Location(1, 4), Amber), Amphipod(Location(3, 7), Amber))) -> 4,
        State(Set(Amphipod(Location(1, 8), Amber), Amphipod(Location(3, 7), Amber))) -> 2,
        State(Set(Amphipod(Location(1, 1), Amber), Amphipod(Location(3, 7), Amber))) -> 7,
        State(Set(Amphipod(Location(1, 10), Amber), Amphipod(Location(3, 7), Amber))) -> 4,
        State(Set(Amphipod(Location(1, 6), Amber), Amphipod(Location(3, 7), Amber))) -> 2,
        State(Set(Amphipod(Location(1, 11), Amber), Amphipod(Location(3, 7), Amber))) -> 5,
        State(Set(Amphipod(Location(1, 2), Amber), Amphipod(Location(3, 7), Amber))) -> 6,
      ))
    }

    "not return states if amphipod is already in destination room" in {
      val state = State(amphipods = Set(
        Amphipod(Location(3, 7), Copper)
      ))

      val nextStates = state.nextStates()

      nextStates must beEqualTo(Map.empty)
    }

    "return cost based on energy of amphipod moving from side room to hallway" in {
      val state = State(amphipods = Set(
        Amphipod(Location(3, 7), Bronze)
      ))

      val nextStates = state.nextStates()

      nextStates must beEqualTo(Map(
        State(Set(Amphipod(Location(1, 4), Bronze))) -> 50,
        State(Set(Amphipod(Location(1, 8), Bronze))) -> 30,
        State(Set(Amphipod(Location(1, 1), Bronze))) -> 80,
        State(Set(Amphipod(Location(1, 10), Bronze))) -> 50,
        State(Set(Amphipod(Location(1, 6), Bronze))) -> 30,
        State(Set(Amphipod(Location(1, 11), Bronze))) -> 60,
        State(Set(Amphipod(Location(1, 2), Bronze))) -> 70
      ))
    }

    "return next states for complex case in" in {
      val state = State(amphipods = Set(
        Amphipod(Location(1, 4), Amber),
        Amphipod(Location(3, 3), Bronze),
        Amphipod(Location(1, 11), Amber),
        Amphipod(Location(2, 7), Desert),
      ))

      val nextStates = state.nextStates()

      nextStates must beEqualTo(Map(
        State(Set(
          Amphipod(Location(1, 4), Amber),
          Amphipod(Location(1, 1), Bronze),
          Amphipod(Location(1, 11), Amber),
          Amphipod(Location(2, 7), Desert)
        )) -> 40,
        State(Set(
          Amphipod(Location(1, 4), Amber),
          Amphipod(Location(1, 2), Bronze),
          Amphipod(Location(1, 11), Amber),
          Amphipod(Location(2, 7), Desert)
        )) -> 30,
        State(amphipods = Set(
          Amphipod(Location(1, 4), Amber),
          Amphipod(Location(3, 3), Bronze),
          Amphipod(Location(1, 11), Amber),
          Amphipod(Location(1, 6), Desert),
        )) -> 2000,
        State(amphipods = Set(
          Amphipod(Location(1, 4), Amber),
          Amphipod(Location(3, 3), Bronze),
          Amphipod(Location(1, 11), Amber),
          Amphipod(Location(1, 8), Desert),
        )) -> 2000,
        State(amphipods = Set(
          Amphipod(Location(1, 4), Amber),
          Amphipod(Location(3, 3), Bronze),
          Amphipod(Location(1, 11), Amber),
          Amphipod(Location(1, 10), Desert),
        )) -> 4000
      ))
    }

    "return states for amphipod returning to second row of side room" in {
      val state = State(amphipods = Set(
        Amphipod(Location(2, 3), Amber),
        Amphipod(Location(1, 10), Amber),
      ))

      val nextStates = state.nextStates()

      nextStates must beEqualTo(Map(
        State(Set(Amphipod(Location(3, 3), Amber), Amphipod(Location(2, 3), Amber))) -> 9,
      ))
    }

    "return states for amphipod returning to first row of side room" in {
      val state = State(amphipods = Set(
        Amphipod(Location(1, 10), Amber),
      ))

      val nextStates = state.nextStates()

      nextStates must beEqualTo(Map(
        State(Set(Amphipod(Location(2, 3), Amber))) -> 8,
      ))
    }

    "not return states if side room contains amphipod of different type" in {
      val state = State(amphipods = Set(
        Amphipod(Location(3, 3), Bronze),
        Amphipod(Location(1, 4), Amber),
        Amphipod(Location(1, 1), Copper),
        Amphipod(Location(1, 2), Copper),
      ))

      val nextStates = state.nextStates()

      nextStates must beEqualTo(Map.empty)
    }

    "not return states if amphipod is blocked from moving to side room" in {
      val state = State(amphipods = Set(
        Amphipod(Location(1, 6), Amber),
        Amphipod(Location(1, 4), Copper),
      ))

      val nextStates = state.nextStates()

      nextStates must beEqualTo(Map.empty)
    }

    "return cost based on energy of amphipod moving from hallway to side room" in {
      val state = State(amphipods = Set(
        Amphipod(Location(1, 1), Desert)
      ))

      val nextStates = state.nextStates()

      nextStates must beEqualTo(Map(
        State(Set(Amphipod(Location(2, 9), Desert))) -> 9000,
      ))
    }
  }

  "calculateLeastEnergyToOrganizeAmphipods" should {

    "return least amount of energy for example in part 1" in {
      val amount = calculateLeastEnergyToOrganizeAmphipods(
        State(amphipods = Set(
          Amphipod(Location(2, 3), Bronze),
          Amphipod(Location(3, 3), Amber),
          Amphipod(Location(2, 5), Copper),
          Amphipod(Location(3, 5), Desert),
          Amphipod(Location(2, 7), Bronze),
          Amphipod(Location(3, 7), Copper),
          Amphipod(Location(2, 9), Desert),
          Amphipod(Location(3, 9), Amber),
        ))
      )

      amount must beEqualTo(12521)
    }

    "return least amount of energy for actual input in part 1" in {
      val amount = calculateLeastEnergyToOrganizeAmphipods(
        State(amphipods = Set(
          Amphipod(Location(2, 3), Desert),
          Amphipod(Location(3, 3), Bronze),
          Amphipod(Location(2, 5), Desert),
          Amphipod(Location(3, 5), Copper),
          Amphipod(Location(2, 7), Bronze),
          Amphipod(Location(3, 7), Amber),
          Amphipod(Location(2, 9), Amber),
          Amphipod(Location(3, 9), Copper),
        ))
      )

      amount must beEqualTo(16244)
    }

    "return least amount of energy for example in part 2" in {
      val amount = calculateLeastEnergyToOrganizeAmphipods(
        State(amphipods = Set(
          Amphipod(Location(2, 3), Bronze),
          Amphipod(Location(3, 3), Desert),
          Amphipod(Location(4, 3), Desert),
          Amphipod(Location(5, 3), Amber),
          Amphipod(Location(2, 5), Copper),
          Amphipod(Location(3, 5), Copper),
          Amphipod(Location(4, 5), Bronze),
          Amphipod(Location(5, 5), Desert),
          Amphipod(Location(2, 7), Bronze),
          Amphipod(Location(3, 7), Bronze),
          Amphipod(Location(4, 7), Amber),
          Amphipod(Location(5, 7), Copper),
          Amphipod(Location(2, 9), Desert),
          Amphipod(Location(3, 9), Amber),
          Amphipod(Location(4, 9), Copper),
          Amphipod(Location(5, 9), Amber),
        ))
      )

      amount must beEqualTo(44169)
    }

    "return least amount of energy for actual input in part 2" in {
      val amount = calculateLeastEnergyToOrganizeAmphipods(
        State(amphipods = Set(
          Amphipod(Location(2, 3), Desert),
          Amphipod(Location(3, 3), Desert),
          Amphipod(Location(4, 3), Desert),
          Amphipod(Location(5, 3), Bronze),
          Amphipod(Location(2, 5), Desert),
          Amphipod(Location(3, 5), Copper),
          Amphipod(Location(4, 5), Bronze),
          Amphipod(Location(5, 5), Copper),
          Amphipod(Location(2, 7), Bronze),
          Amphipod(Location(3, 7), Bronze),
          Amphipod(Location(4, 7), Amber),
          Amphipod(Location(5, 7), Amber),
          Amphipod(Location(2, 9), Amber),
          Amphipod(Location(3, 9), Amber),
          Amphipod(Location(4, 9), Copper),
          Amphipod(Location(5, 9), Copper),
        ))
      )

      amount must beEqualTo(43226)
    }
  }
}
