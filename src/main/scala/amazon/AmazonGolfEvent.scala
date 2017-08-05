package amazon

import java.util.Scanner

import scala.util.Try

/**
 * You are in charge of organizing a golf event for Amazon employees and recently rented out a
 * rectangular field for the event. While the field is close to what it needs to be, it needs
 * some landscaping. The field is flat, except for trees and trenches, and can be represented
 * as a two-dimensional grid. The flat areas are represented as 1, areas with trenches are
 * represented by 0, and areas with trees are represented by numbers greater than 1 indicating
 * their heights (heights are unique). Your goal is to cut down all of the trees to level the
 * field, but you can only cut the shortest tree on the field, at any given time. You can start
 * from any corner of the field, as long as it is flat, and can move one block up, down, left,
 * or right at a time. You cannot walk on trenches, cannot go through a trees, and cannot leave
 * the field. Additionally, once a tree is cut, that area is flat and you move to that area.
 *
 *   Write an algorithm to determine the minimum distance required to level the entire field from the
 * shortest tree to the tallest tree.
 *
 * Input :
 * The input to the function/method consists of three arguments: numRows, an integer
 * representing the number of rows. numColumns, an integer representing the number of columns. field,
 * representing the two-dimensional grid of integers.  Output Return an integer representing the total
 * distance traversed to level the field else return -1.
 *
 *   Constraints
 *  1 ≤ numRows,
 * numColumns ≤ 1000 2
 * ≤ tree height
 *
 *
 */
object AmazonGolfEvent {

  type Field = Array[Array[Int]]
  type Path = List[Pos]
  type Tree = Pos

  def main(args: Array[String]) {
    val sc = new Scanner(System.in)
    val numRows = sc.next.toInt
    val numColumns = sc.next.toInt
    val field = new Array[Array[Int]](numRows)
    val n = for (_ <- 0 to numRows) yield sc.nextLine()
    for (i <- 1 until n.length) { field(i - 1) = n(i).split(" ") map { _.toInt } }
    if (!field.forall(_.length == numColumns)) throw new IllegalStateException()
    val lastTree = findLargestTree(field)
    val answer = findTree(field) match {
      case Some(tree) =>
        val distances = for {
          start <- findStaringPositions(field)
          paths <- findPaths(start, field, tree, List(start))
          if paths.contains(lastTree)
        } yield paths.length
        if (distances.isEmpty) -1 else distances.min
      case None => -1
    }
    println(answer)
  }

  def findLargestTree(field: Field): Pos = {
    var max = 0
    var pos = Pos(0, 0)
    for {
      y <- field.indices
      x <- field.head.indices
      p = Pos(x, y)
    } {
      if (field(p.y)(p.x) > max) {
        pos = p
        max = field(pos.y)(pos.x)
      }
    }
    pos
  }

  def findStaringPositions(field: Field): List[Pos] = {
    val topLeft = Pos(0, 0)
    val topRight = Pos(field.head.length - 1, 0)
    val bottomRight = Pos(0, field.length - 1)
    val bottomLeft = Pos(field.head.length - 1, field.length - 1)
    List(topLeft, topRight, bottomLeft, bottomRight) filter (p => State.FLAT == field(p.y)(p.x))
  }

  def findPaths(pos: Pos, field: Field, tree: Tree, explored: Path): List[Path] = {
    pos.availableMoves(field, field(tree.y)(tree.x), explored.toSet) match {
      case Nil => List(explored)
      case moves =>
        (for {
          move <- moves
        } yield {
          if (move == tree) {
            val f = cutTree(field, tree)
            findTree(f) map { findPaths(move, f, _, List(move)) } getOrElse List(explored :+ move)
          } else findPaths(move, field, tree, explored :+ move)
        }
        ).flatten
    }
  }

  def cutTree(field: Field, tree: Tree): Field =
    field.updated(tree.y, field(tree.y).updated(tree.x, State.FLAT))

  def findTree(field: Field): Option[Tree] = {
    val min =
      for {
        y <- field.indices
        x <- field.head.indices
      } yield Pos(x, y)

    val trees = min filter (p => field(p.y)(p.x) > 1)
    if (trees.isEmpty) None else Some(trees.minBy(t => field(t.y)(t.x)))
  }

  case class Pos(x: Int, y: Int) {
    type Moves = List[Pos]
    type Explored = Set[Pos]

    def UP = Pos(x, y - 1)
    def DOWN = Pos(x, y + 1)
    def RIGHT = Pos(x + 1, y)
    def LEFT = Pos(x - 1, y)

    def moves: Moves = List(UP, DOWN, LEFT, RIGHT)

    def availableMoves(field: Field, treeHeight: Int, explored: Explored): Moves =
      moves filter {
        m =>
          Try {
            checkMoves(m, field, treeHeight) && !explored.contains(m)
          } getOrElse false
      }

    private def checkMoves(move: Pos, field: Field, treeHeight: Int) = {
      val markValue = field(move.y)(move.x)
      if (markValue == State.FLAT) true
      else if (markValue == State.TRENCHES) false
      else State.CUTTABLE_TREE(markValue, treeHeight)

    }
  }

  object State {
    val FLAT = 1
    val TRENCHES = 0
    val CUTTABLE_TREE: (Int, Int) => Boolean = (n: Int, height: Int) => n == height
  }

}

