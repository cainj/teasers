package hackerrank.algorithms.graphtheory

import scala.util.{ Success, Try }

object FrogMaze {

  type Maze = Array[Array[Char]]
  type Tunnels = Array[Tunnel]

  def main(args: Array[String]) {
    val sc = new java.util.Scanner(System.in)
    val n = sc.nextInt()
    val m = sc.nextInt()
    val k = sc.nextInt()
    val maze = Array.ofDim[Char](n, m)
    val tunnels = new Array[Tunnel](k)
    for (i <- 0 until n) {
      maze(i) = sc.next.toCharArray
    }

    for (i <- 0 until k) {
      tunnels(i) = Tunnel((sc.nextInt(), sc.nextInt()), (sc.nextInt(), sc.nextInt()))
    }

    tunnels.foreach { x =>
      println(x)
    }
  }

  case class Cell(x: Int, y: Int) {

    def LEFT = Cell(x - 1, y)
    def RIGHT = Cell(x + 1, y)
    def UP = Cell(x, y - 1)
    def DOWN = Cell(x, y + 1)

    private[this] def moves = List(LEFT, RIGHT, UP, DOWN)

    def neighbors(matrix: Maze): List[Cell] = moves filter {
      move =>
        Try { matrix(move.x)(move.y) } match {
          case Success(c) if c == Cell.Free || c == Cell.Exit => true
          case _ => false
        }
    }

  }

  object Cell {
    val Obstacle = '#'
    val Free = '0'
    val Alef = 'A'
    val Mine = '*'
    val Exit = '%'
  }

  case class Tunnel(x: (Int, Int), y: (Int, Int))
}

