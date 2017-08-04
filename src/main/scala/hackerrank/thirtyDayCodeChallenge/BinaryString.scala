package hackerrank.thirtyDayCodeChallenge

import java.util.Scanner

/**
 * Inspired by: https://www.hackerrank.com/challenges/30-binary-numbers
 *
 * Given a base- 10 integer, n , convert it to binary (base-2). Then find and print the base-10
 * integer denoting the maximum number of consecutive 1's in n's binary representation.
 */
object BinaryString {

  def toBinaryString(n: Int): Int = {
    var i = n
    var max, counter = 0
    while (i > 0) {
      if (i % 2 == 0) {
        max = Math.max(counter, max)
        counter = 0
      } else counter += 1
      i = i / 2
    }
    max
  }

  def main(args: Array[String]) {
    val sc = new Scanner(System.in)
    val n = sc.nextInt
    val binaryString = toBinaryString(n)
    println(binaryString)
  }
}