package day03

import java.lang.Character.digit
import scala.annotation.tailrec
import scala.io.Source
import scala.util.Using

class LifeSupportRating(report: List[String]):

  private def convert(ch: Char): Int = ch - '0'

  private def numberToInt(number: String): Int =
    val numDigits = number.length
    number
      .zipWithIndex.map { case (d, i) => convert(d) * (1 << (numDigits - i - 1)) }
      .sum

  private def countByBit(numbers: List[String], pos: Int): Map[Char, Int] =
    numbers.groupMapReduce(n => n(pos))(_ => 1)(_ + _)

  private def selectBit(numbers: List[String], pos: Int, select: (Int, Int) => Char) =
    val counts = countByBit(numbers, pos)
    select(counts('0'), counts('1'))

  private def calculateRating(bitSelectFn: (Int, Int) => Char): Int =

    @tailrec
    def calculate(numbers: List[String], pos: Int): Int =
      numbers match
        case n :: Nil => numberToInt(n)
        case _ =>
          val digit = selectBit(numbers, pos, bitSelectFn)
          val filtered = numbers.filter(n => n(pos) == digit)
          calculate(filtered, pos + 1)

    calculate(report, 0)

  private def calculateOxygenGeneratorRating(): Int =
    calculateRating((count0, count1) => if count0 <= count1 then '1' else '0')

  private def calculateCO2ScrubberRating(): Int =
    calculateRating((count0, count1) => if count0 <= count1 then '0' else '1')

  def calculate(): Int = calculateOxygenGeneratorRating() * calculateCO2ScrubberRating()

