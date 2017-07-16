package example

import scala.util.Try

/**
    Diamond Mine is your new favorite game. Its map is represented as an n × n matrix, and the value of each cell corresponds to some property of the map:
    A value ≥ 0 represents a path.
    A value of 1 represents a diamond in a path that can be picked up by the player.
    A value of −1 represents a wall (path obstruction).

    The basic rules for playing Diamond Mine are as follows:
    The player starts at (0, 0) and reaches (n−1, n−1), by moving right (→) or down (↓) through valid path cells.
    When passing through a path cell containing a diamond, the diamond is picked up. Once picked up, the cell becomes an empty path cell (meaning you cannot pick up the same diamond twice).
    If there is no valid path between (0, 0) and (n−1, n−1), then no diamonds can be collected.
    A player wins the game by collecting the maximum number of diamonds possible when following the above rules.

    Complete the collectMax function in your editor. It has 1 parameter: a 2D array of integers, mat, describing the game map. It must return an integer denoting the maximum number of diamonds you can collect in the given Diamond Mine game map.

    Input Format
    The locked stub code in your editor reads the following input from stdin and passes it to your function:
    The first line contains an integer, n, denoting the number of rows in mat.
    The second line contains an integer, n, denoting the number of columns in mat.
    Each line i of the n subsequent lines (where 0 ≤ i < n) contains n space-separated integers describing the respective elements of row i in mat.

    Constraints
    1 ≤ n ≤ 100
    −1 ≤ mati,j ≤ 1

    Output Format
    Your function must return an integer denoting the maximum number of diamonds you can collect in the given map. This is printed to stdout by the locked stub code in your editor.

    Sample 1
    Assume you have the following input that represents a simple 3x3 Diamond Mine game board:
    3
    3
    0 1 -1
    1 0 -1
    1 1 1

    Since the player can ONLY move to the right (→) or down (↓), the maximum score a player can achieve is 4
    by following the path (0,0) ⇒ (1,0) ⇒ (2,0) ⇒ (2, 1) ⇒ (2, 2)

    Sample Input 2
    Now consider another simple input board:

    3
    3
    0 1 1
    1 0 −1
    1 −1 0

    Because there is no way to reach the goal cell (2,2) due to the walls located in cells (2,1) and (1,2),
    there is no way to win. Thus the output result should be 0.

  */
object DiamondMine {

  type Path = List[(Position, Int)]

  val start = (Position(0,0), 0)

  type Terrain = Array[Array[Int]]

  val initialPath = List(start)

  def collectMax(terrain: Terrain): Int = {
    val GOAL = Position(terrain.length - 1, terrain.length - 1)

    /**
      * Moves thru the Terrain
      *
      * @param path The path taken
      * @return
      */
    def move(path: Path): List[Path] = {
      val pos = path.last._1
      if(pos == GOAL) List(path)
      else {
        for {
          neighbor <- pos.legalMoves(terrain)
          p <- move(path :+ neighbor)
        } yield p
      }
    }
    val paths = move(initialPath)
    paths match {
      case Nil => 0
      case _ => paths map { path => path.foldLeft(0){ (sum, next) => sum + next._2 } } max
    }
  }

  def main(args: Array[String]) {

    // Sample 1
    println(collectMax(Array(Array(0, 1, 1), Array(1, 0, -1), Array(1, 1, 1))) == 4)

    //Sample 2
    println(collectMax(Array(Array(0, 1, -1), Array(1, 0, -1), Array(1, -1, 0))) == 0)
  }

  case class Position(x: Int, y: Int){
    import State._

    def RIGHT = Position(this.x + 1, this.y)

    def DOWN = Position(this.x, this.y + 1)

    private def square(pos: Position, terrain: Terrain): Int = Try{ terrain(pos.x)(pos.y) } getOrElse(-1)

    def legalMoves(terrain: Terrain): List[(Position, Int)] = List(RIGHT, DOWN) collect {
      case p if(square(p, terrain) != Wall)=> (p, square(p, terrain))
    }
  }

  object State {

    val Wall = -1
    val Diamond = 1
    val Empty = 0
  }
}


