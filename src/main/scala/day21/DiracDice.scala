package day21

import scala.annotation.tailrec

class DiracDice(start1: Int, start2: Int):

  def part1(): Int =

    @tailrec
    def play(space: Map[Int, Int], score: Map[Int, Int], roll: Int, prevPlayer: Int): Int =
      if score(prevPlayer) >= 1000 then
        score.values.min * (roll - 1)
      else
        val player = 1 - prevPlayer
        val nextSpace = (space(player) + (3 * roll) + 3) % 10
        val nextScore = score(player) + nextSpace + 1
        play(space + (player -> nextSpace), score + (player -> nextScore), roll + 3, player)

    play(Map(0 -> (start1 - 1), 1 -> (start2 - 1)), Map(0 -> 0, 1 -> 0), 1, 1)


  def part2(): Long =
    val rollFreq = List(3 -> 1, 4 -> 3, 5 -> 6, 6 -> 7, 7 -> 6, 8 -> 3, 9 -> 1)

    def countWins(space1: Int, score1: Int, space2: Int, score2: Int): (Long, Long) =
      if score2 >= 21 then
        (0, 1)
      else
        val w =
          for (roll, freq) <- rollFreq
              nextSpace = (space1 + roll) % 10
              (c2, c1) = countWins(space2, score2, nextSpace, score1 + nextSpace + 1)
          yield (freq * c1, freq * c2)
        (w.map(_._1).sum, w.map(_._2).sum)

    val (w1, w2) = countWins(start1 - 1, 0, start2 - 1, 0)
    w1.max(w2)


@main
def main(): Unit =
  val dice = new DiracDice(2, 1)
  println(s"Part 1: ${dice.part1()}")
  println(s"Part 2: ${dice.part2()}")