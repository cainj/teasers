package hackerrank.algorithms.sort

class QuickSortInPlace {

  def main(args: Array[String]) {
    val sc = new java.util.Scanner(System.in)
    val n = sc.nextInt()
    val a = new Array[Int](n)
    for (i <- 0 to n - 1) {
      a(i) = sc.nextInt()
    }
    quicksort(a, 0, a.length - 1)
  }

  def partition(array: Array[Int], lo: Int, hi: Int): Int = {
    val pivot = array(hi)
    var i = lo - 1
    for (j <- lo until hi; if (array(j) < pivot)) {
      i += 1
      swap(array, i, j)
    }
    if (array(hi) < array(i + 1)) swap(array, i + 1, hi)
    println(array mkString (" "))
    i + 1
  }

  def swap(a: Array[Int], i: Int, j: Int) = {
    val hold = a(i)
    a(i) = a(j)
    a(j) = hold
  }

  def quicksort(array: Array[Int], lo: Int, hi: Int) {
    if (lo < hi) {
      val pivot: Int = partition(array, lo, hi)
      quicksort(array, lo, pivot - 1)
      quicksort(array, (pivot + 1), hi)
    }
  }
}
