package day12

import scala.annotation.tailrec
import scala.collection.immutable.Queue
import scala.io.Source
import scala.util.Using
import scala.util.chaining.scalaUtilChainingOps

class PassagePathing(input: String):

  private val adjacencyList: Map[String, List[String]] =
    input.linesIterator
      .flatMap {
        case s"$v-$w" => Seq(v -> w, w -> v)
      }
      .toList
      .groupMap((v, _) => v)((_, w) => w)

  private def isLarge(cave: String): Boolean = cave.forall(_.isUpper)

  private def countPaths(path: Seq[String], secondVisitCheck: Seq[String] => Boolean): Int =
    path.head match
      case "end" => 1
      case cave => adjacencyList(cave).collect {
        case c if (c != "start") && (isLarge(c) || !path.contains(c) || secondVisitCheck(path)) =>
          countPaths(c +: path, secondVisitCheck)
      }.sum

  def part1(): Int = countPaths(Seq("start"), _ => false)

  def part2(): Int = countPaths(Seq("start"), _.filterNot(isLarge).pipe(path => path.size == path.distinct.size))
end PassagePathing

@main
def main(): Unit =
  def readInput(): String =
    Using.resource(getClass.getResourceAsStream("/day12.txt")) { stream =>
      Source.fromInputStream(stream).mkString
    }

  val passagePathing = new PassagePathing(readInput())
  println(s"Part 1: ${passagePathing.part1()}")
  println(s"Part 2: ${passagePathing.part2()}")
