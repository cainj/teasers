package hackerrank.algorithms.dynamic_programming

object MaximumSubarray {

  def maxSubarray(arr: Array[Int]): Array[Int] = {
    var max = arr(0)
    var sum = 0
    var sequence = 0
    var currentMaxElement = arr(0)
    for {
      i <- arr.indices
    } yield {
      currentMaxElement = math.max(currentMaxElement, arr(i))
      if (sum + arr(i) < 0) {
        sum = 0
        max = math.max(currentMaxElement, max)
      } else {
        sum += arr(i)
        max = math.max(sum, max)
        if (arr(i) > 0) sequence += arr(i)
      }
    }
    Array(max, if (max < 0) max else sequence)
  }

  def main(args: Array[String]) {
    val sc = new java.util.Scanner(System.in);
    var t = sc.nextInt();
    var a0 = 0;
    while (a0 < t) {
      var n = sc.nextInt();
      var arr = new Array[Int](n);
      for (arr_i <- 0 to n - 1) {
        arr(arr_i) = sc.nextInt();
      }
      val result = maxSubarray(arr);
      println(result.mkString(" "))

      a0 += 1;
    }
  }
}
