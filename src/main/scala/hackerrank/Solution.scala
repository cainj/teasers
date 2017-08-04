package hackerrank

/**
  * https://www.hackerrank.com/challenges/30-2d-arrays/problem
  *
  * Objective
  * Today, we're building on our knowledge of Arrays by adding another dimension. Check out the Tutorial tab for learning materials and an instructional video!
  * Context
  * Given a  2D Array, :
  * 1 1 1 0 0 0
  * 0 1 0 0 0 0
  * 1 1 1 0 0 0
  * 0 0 0 0 0 0
  * 0 0 0 0 0 0
  * 0 0 0 0 0 0
  * We define an hourglass in  to be a subset of values with indices falling in this pattern in 's graphical representation:
      a b c
       d
     e f g
  *
  * There are  hourglasses in A, and an hourglass sum is the sum of an hourglass' values.
  * Task
  * Calculate the hourglass sum for every hourglass in A, then print the maximum hourglass sum.
  */
object Solution {

  type Grid = Array[Array[Int]]

  def main(args: Array[String]) {
    val sc = new java.util.Scanner(System.in)
    val grid = new Array[Array[Int]](6)
    for (i <- 0 until 6) { grid(i) = sc.nextLine().split(" ") map { _.toInt } }
    val hourGlasses: List[HourGlass] = traverse(grid)
    println(hourGlasses map { _.sum } max)
  }

  def traverse(grid: Grid): List[HourGlass] = {
    val slice = grid.length / 2
    (for {
      y <- 0 to slice
      x <- 0 to slice
    } yield {
      val g = Array[Array[Int]](new Array(3), new Array(3), new Array(3))
      for {
        j <- 0 until slice
        i <- 0 until slice
      } yield g(j)(i) = grid(y + j)(x + i)
      createHourGlass(g, slice)
    }).toList
  }

  private def createHourGlass(threeByThree: Grid, slice: Int): HourGlass = {
    val top = for { i <- 0 until slice} yield threeByThree(0)(i)
    val bottom = for { i <- 0 until slice } yield threeByThree(2)(i)
    val middle = threeByThree(1)(1)
    HourGlass(top.toArray, middle, bottom.toArray)
  }

  case class HourGlass(top: Array[Int], middle: Int, bottom: Array[Int]) {
    def sum: Int = top.sum + middle + bottom.sum

    override def toString =
      s""" ${top mkString (" ")}
         |   $middle
         | ${bottom mkString (" ")}
       """.stripMargin
  }
}
