package hackerrank.algorithms.search

import scala.util.{ Success, Try }

/**
 * Problem statement found here.
 *
 * https://www.hackerrank.com/challenges/connected-cell-in-a-grid/problem
 */
object ConnectedCells {

  type Matrix = Array[Array[Int]]
  type Path = Set[Cell]
  type Pos = (Int, Int)

  private[this] def searchPaths(cell: Cell, matrix: Matrix): Path = {
    if (cell.neighbors(matrix).isEmpty) Set(cell)
    else {
      matrix(cell.x)(cell.y) = -1
      cell :: cell.neighbors(matrix).flatMap { c => searchPaths(c, matrix) } toSet
    }
  }

  def connectedCell(matrix: Matrix): Int = {
    val solutions = for {
      x <- matrix.indices
      y <- matrix(x).indices
      if matrix(x)(y) == 1
      cell = Cell(x, y)
    } yield searchPaths(cell, matrix)
    solutions.map { _.size } max
  }

  def main(args: Array[String]) {
    val sc = new java.util.Scanner(System.in)
    val n = sc.nextInt()
    val m = sc.nextInt()
    val matrix = Array.ofDim[Int](n, m)

    for {
      i <- 0 until n
      j <- 0 until m
    } yield matrix(i)(j) = sc.nextInt()

    val result = connectedCell(matrix)
    println(result)
  }

  case class Cell(x: Int, y: Int) {

    def LEFT = Cell(x - 1, y)
    def RIGHT = Cell(x + 1, y)
    def UP = Cell(x, y - 1)
    def DOWN = Cell(x, y + 1)
    def UP_LEFT = Cell(x - 1, y - 1)
    def UP_RIGHT = Cell(x + 1, y - 1)
    def DOWN_LEFT = Cell(x - 1, y + 1)
    def DOWN_RIGHT = Cell(x + 1, y + 1)

    private[this] def moves = List(LEFT, RIGHT, UP, DOWN, UP_LEFT, UP_RIGHT, DOWN_LEFT, DOWN_RIGHT)

    def neighbors(matrix: Matrix): List[Cell] = moves filter {
      move =>
        Try { matrix(move.x)(move.y) } match {
          case Success(z) if z == 1 => true
          case _ => false
        }
    }
  }
}
