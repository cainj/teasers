package codility

import scala.util.Try

/**
 * Coding example by https://codility.com
 * A zero-indexed array A consisting of N different integers is given. The array contains integers in the range [1..(N + 1)], which means that exactly one element is missing.
 *
 * Your goal is to find that missing element.
 *
 * Write a function:
 *
 * object Solution { def solution(a: Array[Int]): Int }
 *
 * that, given a zero-indexed array A, returns the value of the missing element.
 *
 * For example, given array A such that:
 *
 * A[0] = 2
 * A[1] = 3
 * A[2] = 1
 * A[3] = 5
 * the function should return 4, as it is the missing element.
 *
 * Assume that:
 *
 * N is an integer within the range [0..100,000];
 * the elements of A are all distinct;
 * each element of array A is an integer within the range [1..(N + 1)].
 * Complexity:
 *
 * expected worst-case time complexity is O(N);
 * expected worst-case space complexity is O(1), beyond input storage (not counting the storage required for input arguments).
 * Elements of input arrays can be modified.
 */
object PermMiss {

  def quickSort(xs: Array[Int]) = {
    def swap(i: Int, j: Int) {
      val t = xs(i); xs(i) = xs(j); xs(j) = t
    }
    def sort1(l: Int, r: Int) {
      val pivot = xs((l + r) / 2)
      var i = l; var j = r
      while (i <= j) {
        while (xs(i) < pivot) i += 1
        while (xs(j) > pivot) j -= 1
        if (i <= j) {
          swap(i, j)
          i += 1
          j -= 1
        }
      }
      if (l < j) sort1(l, j)
      if (j < r) sort1(i, r)
    }
    sort1(0, xs.length - 1)
  }

  def solution(a: Array[Int]): Int = {
    if (a.isEmpty)
      1
    else
      quickSort(a)
    var i = 1
    i = Try { while (i == a(i - 1) && i < a.length + 1) i += 1 } match {
      case _ => i
    }
    i
  }

  def main(args: Array[String]): Unit = {
    //Should pass
    assert(solution(Array(2, 3, 1, 5)) == 4)

    assert(solution(Array(10, 2, 3, 1, 5, 6, 7, 4, 8)) == 9)

    assert(solution(Array(1, 2, 3, 4)) == 5)

  }
}
