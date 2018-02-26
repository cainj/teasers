package hackerrank.algorithms.graphtheory

import scala.util.{ Success, Try }

object FrogMaze {

  type Maze = Array[Array[Char]]
  type Tunnels = Array[Tunnel]
  type Path = List[Cell]

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
      tunnels(i) = Tunnel(Cell(sc.nextInt() - 1, sc.nextInt() - 1), Cell(sc.nextInt() - 1, sc.nextInt() - 1))
    }

    val alef = findAlef(maze)
    def calculateProbability(cell: Cell, paths: Path, explored: Set[Cell], accum: Int = 0, cameFromTunnel: Boolean = false): List[Long] = {
      // if the visited cell takes Alef is in the tunnel then transport to the landing cell
      // else
      //   if the visited cell is game over add to the lose column
      //   else if the visited cell is exit add path to the win column
      //   else search all possible moves and calculateProbability
      if (inTunnel(cell, tunnels) && !explored.contains(transport(cell, tunnels)))
        calculateProbability(transport(cell, tunnels), paths :+ cell, explored + cell, cameFromTunnel = true)
      else {
        val neighbors = cell.neighbors(maze) diff explored.toList
        if (maze(cell.x)(cell.y) == Cell.Exit) List(1)
        else if (gameOver(cell, maze)) List(0)
        else if (neighbors.isEmpty) if (cameFromTunnel) List(0) else List(2)
        else {
          (for {
            neighbor <- neighbors
          } yield calculateProbability(neighbor, paths :+ cell, explored + cell)).flatten
        }
      }
    }
    val tallies = calculateProbability(alef, List.empty, Set(alef))
    val probability = tallies.count(1==).toFloat / tallies.filterNot(2==).length
    println(if (probability.isNaN) 0 else probability)

  }

  private[this] def gameOver(c: Cell, maze: Maze): Boolean = maze(c.x)(c.y) == Cell.Mine
  private[this] def inTunnel(c: Cell, tunnels: Tunnels): Boolean = tunnels.exists(t => t.x == c || t.y == c)
  private[this] def transport(c: Cell, tunnels: Tunnels): Cell = tunnels.find(t => t.x == c || t.y == c) match {
    case Some(tunnel) => if (tunnel.x == c) tunnel.y else tunnel.x
    case None => throw new IllegalStateException("Not in a tunnel")
  }

  def findAlef(maze: Maze): Cell = {
    val x = maze.indexWhere(x => x.contains(Cell.Alef))
    val y = maze(x).indexWhere(Cell.Alef==)
    Cell(x, y)
  }

  case class Cell(x: Int, y: Int) {

    def LEFT = Cell(x, y - 1)
    def RIGHT = Cell(x, y + 1)
    def UP = Cell(x - 1, y)
    def DOWN = Cell(x + 1, y)

    private[this] def moves = List(LEFT, RIGHT, UP, DOWN)

    def neighbors(matrix: Maze): List[Cell] = moves filter {
      move =>
        Try { matrix(move.x)(move.y) } match {
          case Success(c) if c != Cell.Obstacle => true
          case _ => false
        }
    }

  }

  object Cell {
    val Obstacle = '#'
    val Alef = 'A'
    val Mine = '*'
    val Exit = '%'
  }

  case class Tunnel(x: Cell, y: Cell)
}

