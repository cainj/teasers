package coursera.algos.week3

object QuickSort {

  def scalaQuickSort(list: Array[Int]): Array[Int] = {
    if (list.length < 1) list
    else {
      val pivot = list(list.length / 2)
      Array.concat(scalaQuickSort(list.filter(pivot >)), list.filter(pivot ==), scalaQuickSort(list.filter(pivot <)))
    }
  }

  def quickSort(xs: Array[Int], l: Int, r: Int) {
    if (l < r) {
      val pivot: Int = partition(xs, l, r)
      quickSort(xs, l, pivot - 1)
      quickSort(xs, pivot + 1, r)
    }
  }

  def partition(xs: Array[Int], l: Int, r: Int) = {
    val pivot = xs(l)
    var i = l + 1
    for (j <- l until r; if xs(j) < pivot) {
      swap(xs, i, j)
      i += 1
    }
    i -= 1
    swap(xs, l, i)
    i
  }

  def swap(xs: Array[Int], l: Int, r: Int) = {
    val hold = xs(r)
    xs(r) = xs(l)
    xs(l) = hold
  }
  def main(args: Array[String]) {
    val array = Array(3, 5, 4, 2, 1, 7, 10, -2, -5, 90, 64)
    quickSort(array, 0, array.length)
    println(array.deep)
  }

}
