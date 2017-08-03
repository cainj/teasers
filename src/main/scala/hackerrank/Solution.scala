package hackerrank
import scala.collection.mutable.Map
/**
 *
 */
object Solution {

  def factorial(n: Int): Int = {
    if (n < 2) 1
    else n * factorial(n - 1)
  }

  def main(args: Array[String]) {
    val sc = new java.util.Scanner(System.in);
    var n = sc.nextInt();
    val result = factorial(n);
    println(result)
  }
}
