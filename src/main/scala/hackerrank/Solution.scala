package hackerrank

import hackerrank.thirtyDayCodeChallenge.TwoDimensionalArray.{ HourGlass, traverse }

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
 * a b c
 * d
 * e f g
 *
 * There are  hourglasses in A, and an hourglass sum is the sum of an hourglass' values.
 * Task
 * Calculate the hourglass sum for every hourglass in A, then print the maximum hourglass sum.
 */
object Solution {

  def powerset[A](s: List[A]) = {
    def powerset_rec(acc: List[Set[A]], remaining: List[A]): List[Set[A]] = remaining match {
      case Nil => acc
      case head :: tail => powerset_rec(acc ++ acc.map(_ + head), tail)
    }
    powerset_rec(List(Set.empty[A]), s.toList)
  }

  def checkWinner(codeList: List[String], shoppingCart: List[String]): Int = {

    println(powerset(shoppingCart))
    0
  }

  def main(args: Array[String]) {
    println(checkWinner(List("apple", "orange"), List("apple", "anything", "orange", "banana")))
  }
}
