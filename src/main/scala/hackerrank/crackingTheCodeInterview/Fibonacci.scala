package hackerrank.crackingTheCodeInterview

/**
  * https://www.hackerrank.com/challenges/ctci-fibonacci-numbers/problem
  */
object Fibonacci {

  def fibonacci(x: Int): Int = {
    if (x == 0) 0
    else if (x == 1) 1
    else fibonacci(x - 1) + fibonacci(x - 2)
  }
  def main(args: Array[String]) {
    /** This will handle the input and output**/
    println(fibonacci(readInt()))
  }
}
