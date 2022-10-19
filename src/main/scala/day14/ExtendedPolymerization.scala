package day14

import scala.io.Source
import scala.util.Using

type ElementPair = (Char, Char)

class ExtendedPolymerization(input: String):

  private val (template, rules) = parseInput()
  private val steps = LazyList.iterate(startFrequencies())(nextStep)

  def calculate(stepCount: Int): Long = calculateDifference(steps(stepCount))

  private def parseInput() =
    val sections = input.split("\n\n")
    val template = sections(0)
    val rules = sections(1).linesIterator.map {
      case s"$l -> $r" => (l(0), l(1)) -> r(0)
    }.toMap
    (template, rules)

  private def startFrequencies() =
    template
      .sliding(2)
      .toList
      .groupMapReduce(grp => (grp.head, grp.last))(_ => 1L)(_ + _)

  private def nextStep(frequencies: Map[ElementPair, Long]) =
    frequencies
      .toList
      .flatMap((pair, count) => {
        val rightHand = rules(pair)
        List((pair(0), rightHand) -> count, (rightHand, pair(1)) -> count)
      })
      .groupMapReduce((pair, _) => pair)((_, count) => count)(_ + _)

  private def calculateDifference(frequencies: Map[ElementPair, Long]) = {
    val elementCounts = frequencies
      .toList
      .map((pair, count) => pair(1) -> count)
      .groupMapReduce((elt, _) => elt)((_, count) => count)(_ + _)
      .values
      .toList
    elementCounts.max - elementCounts.min
  }

@main
def main(): Unit =
  val input = Using.resource(getClass.getResourceAsStream("/day14.txt")) { stream =>
    Source.fromInputStream(stream).mkString
  }
  val extendedPolymerization = new ExtendedPolymerization(input)
  println(s"Part 1: ${extendedPolymerization.calculate(10)}")
  println(s"Part 2: ${extendedPolymerization.calculate(40)}")
