package day18

import scala.annotation.tailrec
import scala.collection.mutable
import scala.io.Source
import scala.util.Using

class SnailFish(input: String):

  private val snailFishNumbers = input.linesIterator.toList

  abstract class Node(var parent: Option[InternalNode] = None)

  case class InternalNode(var left: Node, var right: Node) extends Node

  case class LeafNode(var value: Int) extends Node

  private def parse(snailFishNumber: String): Node =
    def createTree(chars: List[Char]): (Node, List[Char]) =
      chars match
        case '[' :: cs =>
          val (left, remainingAfterLeft) = createTree(cs)
          val (right, remainingAfterRight) = createTree(remainingAfterLeft.tail)
          val node = InternalNode(left, right)
          left.parent = Some(node)
          right.parent = Some(node)
          (node, remainingAfterRight.tail)
        case c :: cs if c.isDigit =>
          (LeafNode(c.asDigit), cs)
        case cs =>
          throw new IllegalStateException(s"Invalid list: $cs")

    val (root, _) = createTree(snailFishNumber.toList)
    root
  end parse

  private def findInternalNodeToExplode(node: Node, depth: Int = 0): Option[InternalNode] =
    node match
      case LeafNode(_) => None
      case i@InternalNode(LeafNode(_), LeafNode(_)) if depth == 4 => Some(i)
      case InternalNode(left, right) =>
        findInternalNodeToExplode(left, depth + 1)
          .orElse(findInternalNodeToExplode(right, depth + 1))

  @tailrec
  private def findLeftMostLeaf(node: Node): LeafNode =
    node match
      case l@LeafNode(_) => l
      case InternalNode(left, _) => findLeftMostLeaf(left)

  @tailrec
  private def findRightMostLeaf(node: Node): LeafNode =
    node match
      case l@LeafNode(_) => l
      case InternalNode(_, right) => findRightMostLeaf(right)

  @tailrec
  private def findPreviousLeaf(node: InternalNode): Option[LeafNode] =
    node.parent match
      case None => None
      case Some(p) if p.left eq node => findPreviousLeaf(p)
      case Some(p) => Some(findRightMostLeaf(p.left))

  @tailrec
  private def findNextLeaf(node: InternalNode): Option[LeafNode] =
    node.parent match
      case None => None
      case Some(p) if p.right eq node => findNextLeaf(p)
      case Some(p) => Some(findLeftMostLeaf(p.right))

  private def tryExplode(root: Node): Boolean =
    findInternalNodeToExplode(root) match
      case None => false
      case Some(node@InternalNode(LeafNode(left), LeafNode(right))) =>
        findPreviousLeaf(node).foreach { l =>
          l.value += left
        }
        findNextLeaf(node).foreach { l =>
          l.value += right
        }
        val zeroLeaf = LeafNode(0)
        node.parent.foreach { p =>
          zeroLeaf.parent = Some(p)
          if p.left eq node then
            p.left = zeroLeaf
          else
            p.right = zeroLeaf
        }
        true
      case node => throw new IllegalStateException(s"Cannot explode $node")
  end tryExplode

  private def findLeafToSplit(node: Node): Option[LeafNode] =
    node match
      case l@LeafNode(value) if value >= 10 => Some(l)
      case LeafNode(_) => None
      case InternalNode(left, right) =>
        findLeafToSplit(left).orElse(findLeafToSplit(right))

  private def split(leaf: LeafNode): Boolean =
    val left = LeafNode(leaf.value / 2)
    val right = LeafNode(leaf.value - left.value)
    val node = InternalNode(left, right)
    left.parent = Some(node)
    right.parent = Some(node)
    leaf.parent.foreach { p =>
      node.parent = Some(p)
      if p.left eq leaf then
        p.left = node
      else
        p.right = node
    }
    true

  private def trySplit(root: Node): Boolean =
    findLeafToSplit(root) match
      case None => false
      case Some(leaf) => split(leaf)

  private def singleReduce(root: Node): Boolean =
    tryExplode(root) || trySplit(root)

  @tailrec
  private def reduceSnailFishNumber(root: Node): Node =
    if singleReduce(root) then
      reduceSnailFishNumber(root)
    else
      root

  private def add(root1: Node, root2: Node): Node =
    val combined = InternalNode(root1, root2)
    root1.parent = Some(combined)
    root2.parent = Some(combined)
    combined

  private def summingSnailFishNumbers(): Node =
    snailFishNumbers.map(parse).reduce { (root1, root2) =>
      reduceSnailFishNumber(add(root1, root2))
    }

  private def magnitude(root: Node): Int =
    root match
      case LeafNode(value) => value
      case InternalNode(left, right) => 3 * magnitude(left) + 2 * magnitude(right)

  def part1(): Int =
    magnitude(summingSnailFishNumbers())

  def part2(): Int =
    val magnitudes =
      for number1 <- snailFishNumbers
          number2 <- snailFishNumbers
          if number1 != number2
          sum = reduceSnailFishNumber(add(parse(number1), parse(number2)))
      yield magnitude(sum)
    magnitudes.max

@main
def main(): Unit =
  val input = Using.resource(getClass.getResourceAsStream("/day18.txt")) { stream =>
    Source.fromInputStream(stream).mkString.strip()
  }
  val snailFish = new SnailFish(input)
  println(s"Part 1: ${snailFish.part1()}")
  println(s"Part 2: ${snailFish.part2()}")
