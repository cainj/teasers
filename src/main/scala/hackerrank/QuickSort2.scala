package hackerrank

/**
 *
 */
object QuickSort2 {

  var swaps = 0
  var shifts = 0

  def main(args: Array[String]) {
    val sc = new java.util.Scanner(System.in)
    val n = sc.nextInt()
    val a = new Array[Int](n)
    for (i <- 0 to n - 1) {
      a(i) = sc.nextInt()
    }
    val x = new Array[Int](n)
    a.copyToArray(x)
    quicksort(x, 0, a.length - 1)
    insertionsort(a)
    println(shifts - swaps)
  }

  def partition(array: Array[Int], lo: Int, hi: Int): Int = {
    val pivot = array(hi)
    var i = lo - 1
    for (j <- lo until hi; if (array(j) < pivot)) {
      i += 1
      swap(array, i, j)
      swaps += 1
    }
    if (array(hi) < array(i + 1))
      swap(array, i + 1, hi)
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
      swaps += 1
    }
  }

  def insertionsort(array: Array[Int]): Unit = {
    for (i <- 1 until array.length; item = array(i)) {
      var j = i - 1
      while (j > -1 && array(j) > item) {
        array(j + 1) = array(j)
        j -= 1
        shifts += 1
      }
      array(j + 1) = item
    }
  }
}

//12 11 13 5 6