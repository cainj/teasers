package codility

/**
 *
 */
object MaxCounters {

  def solution(n: Int, a: Array[Int]): Array[Int] = {
    val maxCounter = n + 1
    val rover = new Array[Int](maxCounter)
    var max = 0
    for (i <- 0 until a.length) {
      if (a(i) == maxCounter && max != 0)
        for (j <- rover.indices) rover(j) = max
      else if (a(i) != maxCounter) {
        val current = rover(a(i)) + 1
        rover(a(i)) = current
        max = Math.max(max, current)
      }
    }
    rover.tail
  }
}
