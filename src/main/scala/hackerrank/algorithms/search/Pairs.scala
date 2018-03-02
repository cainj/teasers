package hackerrank.algorithms.search

import scala.collection.mutable

/**
 * https://www.hackerrank.com/challenges/pairs/problem
 */
object Pairs {

  def pairs(k: Int, arr: Array[Int]): Int = {
    var sum = mutable.HashMap.empty[Int, Int]
    var diff = mutable.HashMap.empty[Int, Int]
    arr foreach { num =>
      if (num - k > 0) diff += (num - k -> num)
      sum += (num -> (num + k))
    }
    diff.toList.intersect(sum.toList).length
  }

  def main(args: Array[String]) {
    val sc = new java.util.Scanner(System.in)
    val n = sc.nextInt()
    val k = sc.nextInt()
    val arr = new Array[Int](n)
    for (arr_i <- 0 until n) {
      arr(arr_i) = sc.nextInt()
    }
    val result = pairs(k, arr)
    println(result)
  }
}

