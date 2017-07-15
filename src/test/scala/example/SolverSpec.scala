package example

import org.scalatest.{Matchers, FlatSpec}

/**
  *
  */
class SolverSpec extends FlatSpec with Matchers {
  "The Solver object" should "create a grid from a string" in {
    val problem = new Problem()
    problem.createGrid(grid) shouldEqual Vector(Vector("r", "g", "b", "r"), Vector("g", "b", "r", "r"), Vector("r", "g", "b", "g"))
  }

  "The Solver object" should "create a grid from a string" in {
    val solver = new Solver()
    val problem = new Problem()
    val grid = problem.createGrid(grid)
    solver.findNeighbors(Pos(3,0), grid) shouldEqual List(Pos(3,1), Pos(2,1))
  }


  val grid =
    """|r g b r
       |g b r r
       |r g b g""".stripMargin
}

