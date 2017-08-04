package hackerrank.thirtyDayCodeChallenge

import java.util.Scanner

/**
 * Inspired by: https://www.hackerrank.com/challenges/30-binary-numbers
 *
 * Given a base- 10 integer, n , convert it to binary (base-2). Then find and print the base-10
  * integer denoting the maximum number of consecutive 1's in n's binary representation.
 */
object BinaryString {

  def toBinaryString(n: Int): String = {
    var s = ""
    var i = n
    while (i > 0) {
      s = (if ((i % 2) == 0) "0" else "1") + s
      i = i / 2
    }
    s
  }

  def consecutive(s: String): Int = {
    var i, j, counter, max = 0
    while (i != s.length) {
      if (s(i) == '1') {
        j = i + 1
        counter += 1
        max = Math.max(counter, max)
        while (j < s.length && j != i) {
          if (s(j) == '1') {
            counter += 1
            j += 1
          } else i = j
        }
        max = Math.max(counter, max)
        counter = 0
        i += 1
      } else i += 1
    }
    max
  }

  def main(args: Array[String]) {
    val sc = new Scanner(System.in)
    val n = sc.nextInt
    val binaryString = toBinaryString(n)
    println(consecutive(binaryString))
  }
}