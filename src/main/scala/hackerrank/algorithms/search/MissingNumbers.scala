package hackerrank.algorithms.search

object MissingNumbers {

  def missingNumbers(arr: Array[Int], brr: Array[Int]): Array[Int] = {

    def findMissingNumbers(x: Array[Int], y: Array[Int]) = {
      val lookup = x.foldLeft(Map.empty[Int, Int]) { (m, next) =>
        val c = m.getOrElse(next, 0) + 1
        m + (next -> c)
      }

      y.foldLeft(lookup) { (acc, next) =>
        val c = acc.getOrElse(next, 0) - 1
        if (c == 0) acc - next
        else acc + (next -> c)
      }.keys.toArray.sorted
    }

    if (arr.length < brr.length) findMissingNumbers(brr, arr)
    else findMissingNumbers(arr, brr)
  }

  def main(args: Array[String]) {
    val sc = new java.util.Scanner(System.in)
    val n = sc.nextInt()
    val arr = new Array[Int](n)
    for (arr_i <- 0 until n) {
      arr(arr_i) = sc.nextInt()
    }
    var m = sc.nextInt()
    val brr = new Array[Int](m)
    for (brr_i <- 0 until m) {
      brr(brr_i) = sc.nextInt()
    }
    val result = missingNumbers(arr, brr)
    println(result.mkString(" "))

  }
}

