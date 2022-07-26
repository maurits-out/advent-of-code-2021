package day03

import java.lang.Character.digit
import scala.collection.mutable
import scala.io.Source
import scala.util.Using

class PowerConsumption(report: List[String]):

  private def extractBinaryDigit(number: String, position: Int): Int = number.charAt(position) - '0'

  private def countByBit(position: Int): Map[Int, Int] =
    report.groupMapReduce(extractBinaryDigit(_, position))(_ => 1)(_ + _)

  private def calculateRate(extractBitFn: Int => Int): Int =
    val numBits = report.head.length
    (0 until numBits)
      .map(extractBitFn)
      .zipWithIndex
      .map { case (d, i) => d * (1 << (numBits - i - 1)) }
      .sum

  private def extractMostCommonBit(position: Int): Int =
    countByBit(position).maxBy(_._2)._1

  private def extractLeastCommonBit(position: Int): Int =
    countByBit(position).minBy(_._2)._1

  def calculate(): Int =
    val gammaRate = calculateRate(extractMostCommonBit)
    val epsilonRate = calculateRate(extractLeastCommonBit)
    gammaRate * epsilonRate
