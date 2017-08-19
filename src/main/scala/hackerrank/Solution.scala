package hackerrank

import java.util.Scanner

object Solution {

  def checkPrime(i: Long) =
    if (i == 1) println("Not prime")
    else
      (2 to Math.sqrt(i.toDouble).toInt) collectFirst {
        case n if (i % n == 0) => n
      } match {
        case Some(_) => println("Not prime")
        case None => println("Prime")
      }

  def main(args: Array[String]) {
    val sc = new Scanner(System.in)
    val n = sc.nextLine().trim.toInt
    val primes = new Array[Long](n)
    for (i <- 0 until n) { primes(i) = sc.nextLine().trim.toLong }
    for {
      i <- primes.indices
    } { checkPrime(primes(i)) }
  }
}
