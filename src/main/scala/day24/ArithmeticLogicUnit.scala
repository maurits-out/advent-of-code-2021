package day24

import scala.annotation.tailrec
import scala.io.Source
import scala.util.Using

case class State(input: Seq[Char],
                 vars: Map[Char, Integer] = Map().withDefault(_ => 0))


class ArithmeticLogicUni(input: String):

  private def evaluateOp(operand: String, vars: Map[String, Int]) =
    if operand.forall(_.isDigit) then
      operand.toInt
    else
      vars(operand)
  end evaluateOp

  @tailrec
  private def evaluate(instructions: Seq[String], input: Seq[Char], vars: Map[String, Int]): Boolean =
    instructions match
      case Seq() =>
        vars("z") == 0
      case current :: remaining =>
        val components = current.split(' ')
        components(0) match
          case "inp" =>
            print((vars("z")) + "-")
            evaluate(remaining, input.tail, vars + (components(1) -> input.head.asDigit))
          case "add" =>
            val res = evaluateOp(components(1), vars) + evaluateOp(components(2), vars)
            evaluate(remaining, input, vars + (components(1) -> res))
          case "mul" =>
            val res = evaluateOp(components(1), vars) * evaluateOp(components(2), vars)
            evaluate(remaining, input, vars + (components(1) -> res))
          case "div" =>
            val res = evaluateOp(components(1), vars) / evaluateOp(components(2), vars)
            evaluate(remaining, input, vars + (components(1) -> res))
          case "mod" =>
            val res = evaluateOp(components(1), vars) % evaluateOp(components(2), vars)
            evaluate(remaining, input, vars + (components(1) -> res))
          case "eql" =>
            val res = if evaluateOp(components(1), vars) == evaluateOp(components(2), vars) then 1 else 0
            evaluate(remaining, input, vars + (components(1) -> res))
  end evaluate

  def isValid(modelNumber: String): Boolean =
    evaluate(input.linesIterator.toList, modelNumber.toSeq, Map().withDefault(_ => 0))

@main
def main(): Unit =
  val input = Using.resource(getClass.getResourceAsStream("/day24.txt")) { stream =>
    Source.fromInputStream(stream).mkString
  }
  val alu = new ArithmeticLogicUni(input)
  println(alu.isValid("12345678912345"))
  println(alu.isValid("11111111111111"))
