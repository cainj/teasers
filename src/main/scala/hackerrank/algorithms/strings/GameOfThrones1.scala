package hackerrank.algorithms.strings

import scala.annotation.tailrec

/**
 * https://www.hackerrank.com/challenges/game-of-thrones
 */
object GameOfThrones1 {

  def main(args: Array[String]) {
    val sc = new java.util.Scanner(System.in);
    val s = sc.next();
    val m = s.foldLeft(Map.empty[Char, Int]) {
      (rover, c) =>
        val x = rover.getOrElse(c, 0)
        rover + (c -> (x + 1))
    }
    val odds = m.count(_._2 % 2 == 1)
    if (odds > 2) println("NO") else println("YES")
  }
}
