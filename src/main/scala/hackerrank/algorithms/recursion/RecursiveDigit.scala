package hackerrank.algorithms.recursion

/**
 * Hacker Rank Recursive Digit
 */
object RecursiveDigit {

  def superDigit(n: String, result: BigInt, initial: Boolean = false): BigInt = {
    if ((initial && n == "") || (n == "" && result < 10)) result
    else if (n == "") superDigit(result.toString, 0, initial)
    else superDigit(n.tail, result + n.head.toString.toInt, initial)
  }

  def main(args: Array[String]) {
    val sc = new java.util.Scanner(System.in)
    val n = sc.next
    val k = sc.nextInt()
    val initial = superDigit(n, 0, true) * k
    println(superDigit(initial.toString, 0))
  }
}
