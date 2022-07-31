package day04

import scala.collection.immutable.Map
import scala.io.Source
import scala.util.Using

type Matrix = List[List[Int]]

def readInput(): String =
  Using.resource(getClass.getResourceAsStream("/day04.txt")) { stream =>
    Source.fromInputStream(stream).mkString
  }

def parseNumbers(sections: Array[String]): List[Int] =
  sections(0).split(',').map(_.toInt).toList

def parseBoards(sections: Array[String]): List[Matrix] =
  def parseBoard(board: String): Matrix =
    board.linesIterator.map(row =>
      row.trim.split(" +").map(_.toInt).toList
    ).toList

  sections.drop(1).map(parseBoard).toList

def parseInput(input: String): (List[Int], List[Matrix]) =
  val sections = input.split("\n\n")
  (parseNumbers(sections), parseBoards(sections))

def boardToTurnMatrix(board: Matrix, numberToTurn: Map[Int, Int]): Matrix =
  board.map(row => row.map(numberToTurn))

def winningTurn(board: Matrix, numberToTurn: Map[Int, Int]): Int =
  val turnMatrix = boardToTurnMatrix(board, numberToTurn)
  (turnMatrix ++ turnMatrix.transpose).map(_.max).min

def summingUnmarked(board: Matrix, lastTurn: Int, numberToTurn: Map[Int, Int]): Int =
  board.flatten.filter(number => numberToTurn(number) > lastTurn).sum

def solve(numbers: List[Int], boards: List[Matrix]): (Int, Int) =
  val numberToTurn = numbers.zipWithIndex.toMap
  val turnToNumber = numberToTurn.map((n, i) => (i, n))
  val winningTurns = boards.map(b => (b, winningTurn(b, numberToTurn)))

  val (winningBoard, lastTurnWinning) = winningTurns.minBy((_, turn) => turn)
  val scoreWinningBoard = summingUnmarked(winningBoard, lastTurnWinning, numberToTurn) * turnToNumber(lastTurnWinning)

  val (losingBoard, lastTurnLosing) = winningTurns.maxBy((_, turn) => turn)
  val scoreLosingBoard = summingUnmarked(losingBoard, lastTurnLosing, numberToTurn) * turnToNumber(lastTurnLosing)

  (scoreWinningBoard, scoreLosingBoard)

@main
def bingo(): Unit =
  val (numbers, boards) = parseInput(readInput())
  val (scoreWinningBoard, scoreLosingBoard) = solve(numbers, boards)
  println(s"Part 1: $scoreWinningBoard")
  println(s"Part 2: $scoreLosingBoard")
