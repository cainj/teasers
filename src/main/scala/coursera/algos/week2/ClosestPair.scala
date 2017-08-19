package coursera.algos.week2

import example.sorting.NumberOfInversions.numberOfInversions

object ClosestPair {

  def mergeSort(xs: Array[Int]): Array[Int] = {

    def sort(xa: Array[Int], ya: Array[Int], acc: Array[Int]): Array[Int] = (xa, ya) match {
      case (Array(), y) => acc ++ y
      case (x, Array()) => acc ++ x
      case (x, y) =>
        if (x.head < y.head) sort(x.tail, y, acc :+ x.head)
        else sort(x, y.tail, acc :+ y.head)
    }

    val n = xs.length / 2
    if (n <= 2) xs
    else {
      val (l, r) = xs.splitAt(n)
      sort(mergeSort(l), mergeSort(r), Array.empty)
    }
  }

  //  def bruteForce(points: Array[Int]): Int = {
  //
  //    val numPoints = points.size
  //    if (numPoints < 2)
  //      return null
  //    var pair = points(0) - points(1)
  //    if (numPoints > 2) {
  //      for (i <- 0 until numPoints - 1) {
  //        val point1 = points(i)
  //        for (j <- i + 1 until numPoints) {
  //          val point2 = points(j)
  //          val distance = point1 - point2
  //          if (distance < pair)
  //            pair = distance
  //        }
  //      }
  //    }
  //    return pair
  //  }

}
