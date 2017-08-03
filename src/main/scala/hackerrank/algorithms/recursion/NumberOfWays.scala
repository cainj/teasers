package hackerrank.algorithms.recursion

class NumberOfWays {

  def numberOfWays(X: Int, N: Int): Int = {
    // Compute the answer in this function over here
    // It is fine to define new functions as and where required
    def doNumberOfWays(v: Int, N: Int, cur: Int): Int = {
      val value = v - scala.math.pow(cur, N).toInt
      value match {
        case 0 => 1
        case j if j < 0 => 0
        case _ => doNumberOfWays(value, N, cur + 1) + doNumberOfWays(v, N, cur + 1)

      }
    }
    doNumberOfWays(X, N, 1)
  }

  def main(args: Array[String]) {
    println(numberOfWays(readInt(), readInt()))
  }
}
