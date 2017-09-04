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
    val center = if (xs.length % 2 == 0) ((left + right) / 2) - 1 else (left + right) / 2
    // order left & center
    if (xs(left) > xs(center)) swap(xs, left, center)
    // order left & right
    if (xs(left) > xs(right)) swap(xs, left, right)
    // order center & right
    if (xs(center) > xs(right)) swap(xs, center, right)

    swap(xs, center, left) // put pivot on left

    xs(left) // return median value
  }

  def quickSort(xs: Array[Int], l: Int, r: Int) {
    if (l < r) {
      val i: Int = partition(xs, l, r)
      counter = (l until r).length + counter
      quickSort(xs, l, i - 1)
      quickSort(xs, i + 1, r)
    }
  }

  def partition(xs: Array[Int], first: Int, last: Int): Int = {
    val pivot = findMedian(xs, first, last)
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
    //val array = Array(4, 5, 8, 3, 2, 1)
    quickSort(array, 0, array.length - 1)
    val end = System.currentTimeMillis()
    //println(array.toList)
    println("Total time: " + (end - start))
    println(counter)
  }

}
