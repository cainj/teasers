package hackerrank.crackingTheCodeInterview

/**
 * https://www.hackerrank.com/challenges/ctci-fibonacci-numbers/problem
 */
object Fibonacci {

  lazy val fib: Stream[Int] = 0 #:: 1 #:: fib.zip(fib.tail).map(p => p._1 + p._2)

  def main(args: Array[String]) {
    /** This will handle the input and output**/
    println(fib take (readInt() + 1) last)
  }
}
