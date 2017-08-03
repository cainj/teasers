package codility

/**
 * https://codility.com/programmers/lessons/4-counting_elements/
 * Task description
 * Write a function:
 *
 * class Solution { public int solution(int[] A); }
 *
 * that, given a non-empty zero-indexed array A of N integers, returns the minimal positive integer (greater than 0) that does not occur in A.
 *
 * For example, given:
 *
 * A[0] = 1
 * A[1] = 3
 * A[2] = 6
 * A[3] = 4
 * A[4] = 1
 * A[5] = 2
 * the function should return 5.
 *
 * Assume that:
 *
 * N is an integer within the range [1..100,000];
 * each element of array A is an integer within the range [−2,147,483,648..2,147,483,647].
 * Complexity:
 *
 * expected worst-case time complexity is O(N);
 * expected worst-case space complexity is O(N), beyond input storage (not counting the storage required for input arguments).
 * Elements of input arrays can be modified.
 */
object MissingInteger {

  def solution(a: Array[Int]): Int = {
    if (a.length == 0) 1
    else {
      quickSort(a)
      val last = a.lastIndexWhere(0 >) + 1
      var num = 0
      for (i <- last until a.length) {
        if (Math.abs(a(i) - num) > 1) return num + 1
        else if (a(i) == num || a(i) - num == 1) num = a(i)
      }
      if (num == a(a.length - 1) || num == 0) num + 1 else num
    }
  }

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

  def main(args: Array[String]): Unit = {
    //Should pass
    println(solution(Array(-1)))

  }

}
