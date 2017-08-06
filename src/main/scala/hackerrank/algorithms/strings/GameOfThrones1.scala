package hackerrank.algorithms.strings

import scala.annotation.tailrec

/**
 * https://www.hackerrank.com/challenges/game-of-thrones
 */
object GameOfThrones1 {

  def main(args: Array[String]) {
    val sc = new java.util.Scanner(System.in);
    val s = sc.next();

    s.toList.permutations.map { _.mkString } find {
      letters => letters.reverse == letters
    } map { _ => println("YES") } getOrElse println("NO")
  }

  /**
   * Without the scala built in version
   * @param chars
   * @return
   */
  def permutation(chars: List[Char]): List[List[Char]] = chars match {
    case List(_) => List(chars)
    case _ =>
      for {
        c <- chars
        (l, r) = chars span { c!= }
        ys <- permutation(l ++ r.tail)
      } yield c :: ys
  }

  /**
    * Not in place O(n) in time O(n +1) in space
    *
    * Without the scala built in version
    * @param chars
    * @return
    */
  def reverse(chars: List[Char]): String = {

    @tailrec
    def doReverse(chars: List[Char], acc: List[Char]): List[Char] = chars match {
      case Nil => acc
      case head :: tail => doReverse(tail, head :: acc)
    }
    doReverse(chars, Nil) mkString
  }
}
