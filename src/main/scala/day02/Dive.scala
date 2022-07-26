package day02

import scala.io.Source
import scala.util.Using

object Dive:

  enum Command:
    case Forward(amount: Int)
    case Down(amount: Int)
    case Up(amount: Int)

  object Command:
    def parse(s: String): Command =
      s match
        case s"forward $x" if x.toIntOption.isDefined => Forward(x.toInt)
        case s"up $x" if x.toIntOption.isDefined => Up(x.toInt)
        case s"down $x" if x.toIntOption.isDefined => Down(x.toInt)

  private case class State(horizontal: Int = 0, depth: Int = 0, aim: Int = 0):
    def outcome: Int = horizontal * depth

  private def readCourse(): List[Command] =
    Using.resource(getClass.getResourceAsStream("/day02.txt")) { stream =>
      Source.fromInputStream(stream).getLines().map(Command.parse).toList
    }

  private def update1(state: State, command: Command): State =
    command match
      case Command.Forward(amount) => state.copy(horizontal = state.horizontal + amount)
      case Command.Down(amount) => state.copy(depth = state.depth + amount)
      case Command.Up(amount) => state.copy(depth = state.depth - amount)

  private def update2(state: State, command: Command): State =
    command match
      case Command.Forward(amount) => state.copy(horizontal = state.horizontal + amount, depth = state.depth + (state.aim * amount))
      case Command.Down(amount) => state.copy(aim = state.aim + amount)
      case Command.Up(amount) => state.copy(aim = state.aim - amount)

  private def followCourse(course: List[Command], update: (State, Command) => State): Int =
    val state = course.foldLeft(State())(update)
    state.outcome

  def main(args: Array[String]): Unit =
    val course = readCourse()
    println(s"Part 1: ${followCourse(course, update1)}")
    println(s"Part 2: ${followCourse(course, update2)}")
