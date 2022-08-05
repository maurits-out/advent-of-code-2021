package day05

import scala.annotation.tailrec
import scala.collection.mutable
import scala.io.Source
import scala.util.Using

object HydrothermalVenture:

  case class Point(x: Int, y: Int):
    def step(d: Point): Point = Point(x + d.x, y + d.y)

  object Point:
    def parse(p: String): Point =
      val strings = p.split(",")
      Point(strings(0).toInt, strings(1).toInt)

  case class Vent(start: Point, end: Point):
    def isHorizontalOrVertical: Boolean = (start.x == end.x) || (start.y == end.y)

    def points: List[Point] =
      val d = Point((end.x - start.x).sign, (end.y - start.y).sign)

      @tailrec
      def points(current: Point, result: List[Point]): List[Point] =
        if current == end then
          result
        else
          val next = current.step(d)
          points(next, next :: result)

      points(start, List(start))
    end points
  end Vent

  object Vent:
    def parse(v: String): Vent =
      val strings = v.split(" -> ")
      Vent(Point.parse(strings(0)), Point.parse(strings(1)))

  private def readInput(): List[Vent] =
    Using.resource(getClass.getResourceAsStream("/day05.txt")) { stream =>
      Source.fromInputStream(stream).getLines().map(Vent.parse).toList
    }

  private def countOverlappingPoints(vents: List[Vent]): Int =
    vents
      .flatMap(_.points)
      .groupMapReduce(identity)(_ => 1)(_ + _)
      .count((_, count) => count >= 2)

  def main(args: Array[String]): Unit =
    val vents = readInput()
    val answer1 = countOverlappingPoints(vents.filter(_.isHorizontalOrVertical))
    val answer2 = countOverlappingPoints(vents)
    println(s"Answer part 1: $answer1")
    println(s"Answer part 2: $answer2")

end HydrothermalVenture
