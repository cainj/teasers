package hackerrank.algorithms.strings.sort

/**
 *
 */
object InsertionSort {

  def main(args: Array[String]) {
    val sc = new java.util.Scanner(System.in)
    val n = sc.nextInt()
    val a = new Array[Int](n)
    for (i <- 0 to n - 1) {
      a(i) = sc.nextInt()
    }

    insertionSort(a)
    println(a mkString (" "))
  }

  def insertionSort(ar: Array[Int]): Unit = {
    for (i <- 1 until ar.length; item = ar(i)) {
      var j = i - 1
      while (j > -1 && ar(j) > item) {
        ar(j + 1) = ar(j)
        j -= 1
      }
      ar(j + 1) = item
    }
  }
}
