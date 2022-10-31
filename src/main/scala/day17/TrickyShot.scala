package day17

import java.lang.Math.max
import scala.annotation.tailrec

class TrickyShot(minX: Int, maxX: Int, minY: Int, maxY: Int):

  private val (targetRangeX, targetRangeY) = (minX to maxX, minY to maxY)

  def part1(): Int = (minY * (minY + 1)) / 2

  def part2(): Int =
    val initialVelocityValues =
      for x <- 0 to maxX
          y <- minY until -minY
          if isInTargetRange(x, y)
      yield (x, y)
    initialVelocityValues.size

  private def isInTargetRange(initVelX: Int, initVelY: Int): Boolean =

    @tailrec
    def iterate(x: Int, y: Int, velX: Int, velY: Int): Boolean =
      if (targetRangeX contains x) && (targetRangeY contains y) then
        true
      else if (x > maxX) || (y < minY) then
        false
      else
        iterate(x + velX, y + velY, max(0, velX - 1), velY - 1)

    iterate(0, 0, initVelX, initVelY)
  end isInTargetRange

@main
def main(): Unit =
  val trickyShot = new TrickyShot(96, 125, -144, -98)
  println(s"Part 1: ${trickyShot.part1()}")
  println(s"Part 2: ${trickyShot.part2()}")
