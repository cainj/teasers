package coursera.algos.week3

import coursera.algos.week2.NumberOfInversions.numberOfInversions

import scala.io.Source

object QuickSort {

  //162085 first
  //164123 second
  //138382

  var counter = 0

  def scalaQuickSort(list: Array[Int]): Array[Int] = {
    if (list.length < 1) list
    else {
      val pivot = list(list.length / 2)
      Array.concat(scalaQuickSort(list.filter(pivot >)), list.filter(pivot ==), scalaQuickSort(list.filter(pivot <)))
    }
  }

  def findMedian(xs: Array[Int], left: Int, right: Int): Int = {
    val first = xs(left)
    val middle = if (xs.length % 2 == 0) xs((right - left - 1) / 2) else xs((right - left) / 2)
    val last = xs(right)

    val max = Math.max(Math.max(first, middle), last)
    val min = Math.min(Math.min(first, middle), last)
    val median = first ^ middle ^ last ^ max ^ min

    if (median == first) left
    else if (median == middle) middle
    else right
  }

  def quickSort(xs: Array[Int], l: Int, r: Int) {
    if (l < r) {
      val median = findMedian(xs, l, r)
      swap(xs, r, l)
      val pivot: Int = partition(xs, l, r)
      counter = (l until r).length + counter
      quickSort(xs, l, pivot - 1)
      quickSort(xs, pivot + 1, r)
    }
  }

  def partition(xs: Array[Int], first: Int, last: Int): Int = {
    val pivot = xs(first)
    println(s"pivot - $pivot")
    var left = first + 1
    for {
      right <- left to last
      if xs(right) < pivot
    } {
      swap(xs, left, right)
      left += 1
    }
    left -= 1
    swap(xs, first, left)
    left
  }

  def swap(xs: Array[Int], l: Int, r: Int) = {
    val hold = xs(r)
    xs(r) = xs(l)
    xs(l) = hold
  }
  def main(args: Array[String]) {
    val fileWithNumbers = "QuickSort.txt"
    val start = System.currentTimeMillis()
    val array = Source.fromResource(fileWithNumbers).getLines().map(Integer.parseInt).toArray
    //    val array = Array(4, 5, 8, 3, 2, 1)
    quickSort(array, 0, array.length - 1)
    val end = System.currentTimeMillis()
    println("Total time: " + (end - start))
    println(array.toList)
    //println(counterLast)
    println(counter)
  }

}
