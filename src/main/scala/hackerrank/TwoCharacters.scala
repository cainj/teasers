package hackerrank

/**
 * https://www.hackerrank.com/challenges/two-characters
 */
object TwoCharacters {

  def main(args: Array[String]) {
    val sc = new java.util.Scanner(System.in);
    var len = sc.nextInt();
    var s = sc.next();
    var chars = List.empty[Char]
    for (c <- s) yield { if (!chars.contains(c)) chars = c :: chars } // can use s.toSet
    if (chars.length == 1) println(0)
    else {
      val combinations = chars.combinations(if (chars.length > 2) chars.length - 2 else 0)
      val combos = for (c <- combinations) yield c.foldLeft(s) { (str, n) => str.replaceAll(n.toString, "") }
      val ans = for (c <- combos; if (valid(c.tail, c.head))) yield c
      println(if (ans.nonEmpty) ans.maxBy(_.length).length else 0)
    }
  }

  def valid(s: String, prev: Char): Boolean = {
    if (s.isEmpty) true
    else if (s.head == prev) false
    else valid(s.tail, s.head)
  }
}
