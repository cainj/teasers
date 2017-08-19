package hackerrank.datastructures.arrays

/**
 *
 * https://www.hackerrank.com/challenges/array-left-rotation
 */
object LeftRotation {

  def leftRotation(a: Array[Int], d: Int): Array[Int] = {
    if (a.length == d) a
    else {
      val pivot = a(d)
      val (l, r) = a.splitAt(pivot)
      r ++ l
    }
  }

  def main(args: Array[String]) {
    val sc = new java.util.Scanner(System.in);
    var n = sc.nextInt();
    var d = sc.nextInt();
    var a = new Array[Int](n);
    for (a_i <- 0 to n - 1) {
      a(a_i) = sc.nextInt();
    }
    val result = leftRotation(a, d);
    println(result.mkString(" "))

  }
}
