package hackerrank

/**
 * https://www.hackerrank.com/challenges/camelcase
 *
 */
object CamelCase {

  def camelCase(s: String, result: Int): Int = {
    if (s.isEmpty) result + 1
    else if (s.head.isUpper) camelCase(s.tail, result + 1)
    else camelCase(s.tail, result)
  }

  def main(args: Array[String]) {
    val sc = new java.util.Scanner(System.in);
    var s = sc.next();
    println(camelCase(s, 0))
  }
}
