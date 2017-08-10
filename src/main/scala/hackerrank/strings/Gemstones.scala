package hackerrank.strings

object Gemstones {

  def main(args: Array[String]) {
    val sc = new java.util.Scanner(System.in);
    var n = sc.nextInt();
    var arr = new Array[String](n);
    for (arr_i <- 0 to n - 1) {
      arr(arr_i) = sc.next();
    }
    val intersects = arr.reduce { (acc, next) => acc.intersect(next) }
    println(intersects.length)
  }
}
