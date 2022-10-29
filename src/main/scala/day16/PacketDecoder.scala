package day16

import scala.annotation.tailrec
import scala.collection.immutable.Seq
import scala.io.Source
import scala.util.Using

trait Packet:
  def version: Int

  def value: Long

case class Literal(version: Int, value: Long) extends Packet

case class SumOperator(version: Int, subPackets: List[Packet]) extends Packet :
  def value: Long = subPackets.map(_.value).sum

case class ProductOperator(version: Int, subPackets: List[Packet]) extends Packet :
  def value: Long = subPackets.map(_.value).product

case class MinimumOperator(version: Int, subPackets: List[Packet]) extends Packet :
  def value: Long = subPackets.map(_.value).min

case class MaximumOperator(version: Int, subPackets: List[Packet]) extends Packet :
  def value: Long = subPackets.map(_.value).max

case class GreaterThanOperator(version: Int, subPackets: List[Packet]) extends Packet :
  def value: Long =
    val List(first, second) = subPackets
    if first.value > second.value then 1 else 0

case class LessThanOperator(version: Int, subPackets: List[Packet]) extends Packet :
  def value: Long =
    val List(first, second) = subPackets
    if first.value < second.value then 1 else 0

case class EqualToOperator(version: Int, subPackets: List[Packet]) extends Packet :
  def value: Long =
    val List(first, second) = subPackets
    if first.value == second.value then 1 else 0

class PacketDecoder(input: String):

  private val hexToBin: Map[Char, String] = Map(
    '0' -> "0000", '1' -> "0001", '2' -> "0010", '3' -> "0011",
    '4' -> "0100", '5' -> "0101", '6' -> "0110", '7' -> "0111",
    '8' -> "1000", '9' -> "1001", 'A' -> "1010", 'B' -> "1011",
    'C' -> "1100", 'D' -> "1101", 'E' -> "1110", 'F' -> "1111")

  private val rootPacket = parseInput()

  private def toDecimal(binaryData: List[Char]): Long =
    binaryData.foldLeft(0L) { (acc, ch) => (acc * 2) + ch.asDigit }

  private def parseLiteralValue(binaryData: List[Char]): (Long, List[Char]) =

    @tailrec
    def parseLiteralValue(binaryData: List[Char], acc: List[Char]): (Long, List[Char]) =
      val (group, binaryDataAfterGroup) = binaryData.splitAt(5)
      group.head match
        case '0' => (toDecimal(acc ++ group.tail), binaryDataAfterGroup)
        case _ => parseLiteralValue(binaryDataAfterGroup, acc ++ group.tail)
    end parseLiteralValue

    parseLiteralValue(binaryData, List.empty)
  end parseLiteralValue

  private def parseMaxBitsSubPackets(binaryData: List[Char]): (List[Packet], List[Char]) =
    val (maxLength, afterMaxLengthBinaryData) = binaryData.splitAt(15)
    val (subPackets, afterSubPacketsBinaryData) = afterMaxLengthBinaryData.splitAt(toDecimal(maxLength).toInt)

    @tailrec
    def parseSubPackets(binaryData: List[Char], acc: List[Packet]): (List[Packet], List[Char]) =
      if binaryData.isEmpty then
        (acc.reverse, afterSubPacketsBinaryData)
      else
        val (subPacket, afterSubPacketBinaryData) = parsePacket(binaryData)
        parseSubPackets(afterSubPacketBinaryData, subPacket :: acc)
    end parseSubPackets

    parseSubPackets(subPackets, List.empty)
  end parseMaxBitsSubPackets

  private def parseNumberOfSubPackets(binaryData: List[Char]): (List[Packet], List[Char]) =

    @tailrec
    def parseSubPackets(binaryData: List[Char], remaining: Int, acc: List[Packet]): (List[Packet], List[Char]) =
      if remaining == 0 then
        (acc.reverse, binaryData)
      else
        val (subPacket, afterSubPacket) = parsePacket(binaryData)
        parseSubPackets(afterSubPacket, remaining - 1, subPacket :: acc)
    end parseSubPackets

    val (packetCount, afterPacketCountBinaryData) = binaryData.splitAt(11)
    parseSubPackets(afterPacketCountBinaryData, toDecimal(packetCount).toInt, List.empty)
  end parseNumberOfSubPackets

  private def parseOperatorSubPackets(binaryData: List[Char]): (List[Packet], List[Char]) =
    val (lengthTypeId, afterLengthTypeBinaryData) = (binaryData.head, binaryData.tail)
    if (lengthTypeId == '0')
      parseMaxBitsSubPackets(afterLengthTypeBinaryData)
    else
      parseNumberOfSubPackets(afterLengthTypeBinaryData)
  end parseOperatorSubPackets

  private def parsePacket(binaryData: List[Char]): (Packet, List[Char]) =
    val (version, afterVersionBinaryData) = binaryData.splitAt(3)
    val (packetTypeId, afterPacketTypeIdBinaryData) = afterVersionBinaryData.splitAt(3)

    val versionDec = toDecimal(version).toInt
    if toDecimal(packetTypeId) == 4 then
      val (value, remaining) = parseLiteralValue(afterPacketTypeIdBinaryData)
      (Literal(versionDec, value), remaining)
    else
      val (subPackets, remaining) = parseOperatorSubPackets(afterPacketTypeIdBinaryData)
      val operator = toDecimal(packetTypeId) match
        case 0 => SumOperator(versionDec, subPackets)
        case 1 => ProductOperator(versionDec, subPackets)
        case 2 => MinimumOperator(versionDec, subPackets)
        case 3 => MaximumOperator(versionDec, subPackets)
        case 5 => GreaterThanOperator(versionDec, subPackets)
        case 6 => LessThanOperator(versionDec, subPackets)
        case 7 => EqualToOperator(versionDec, subPackets)
      (operator, remaining)
  end parsePacket

  private def parseInput(): Packet =
    val binaryData = input.flatMap(hexToBin).toList
    val (packet, _) = parsePacket(binaryData)
    packet
  end parseInput

  private def summingVersion(packet: Packet): Int =
    packet match
      case Literal(version, _) => version
      case SumOperator(version, subPackets) => version + subPackets.map(summingVersion).sum
      case ProductOperator(version, subPackets) => version + subPackets.map(summingVersion).sum
      case MinimumOperator(version, subPackets) => version + subPackets.map(summingVersion).sum
      case MaximumOperator(version, subPackets) => version + subPackets.map(summingVersion).sum
      case GreaterThanOperator(version, subPackets) => version + subPackets.map(summingVersion).sum
      case LessThanOperator(version, subPackets) => version + subPackets.map(summingVersion).sum
      case EqualToOperator(version, subPackets) => version + subPackets.map(summingVersion).sum
  end summingVersion

  def part1(): Int = summingVersion(rootPacket)

  def part2(): Long = rootPacket.value

end PacketDecoder

@main
def main(): Unit =
  val input = Using.resource(getClass.getResourceAsStream("/day16.txt")) { stream =>
    Source.fromInputStream(stream).mkString.strip()
  }
  val decoder = new PacketDecoder(input)
  println(s"Part 1: ${decoder.part1()}")
  println(s"Part 2: ${decoder.part2()}")
