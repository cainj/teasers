package hackerrank.crackingTheCodeInterview

/**
  * https://www.hackerrank.com/challenges/coin-change/problem
  */
object CoinChange {

  def change(n: Int, coins: Array[Int]): Long = {
    val rover = new Array[Long](n + 1)
    rover(0) = 1
    for {
      coin <- coins
      amount <- rover.indices
      if amount >= coin
    } rover(amount) += rover(amount - coin)
    rover(n)
  }

  def main(args: Array[String]) {
    val sc = new java.util.Scanner(System.in)
    val n = sc.nextInt()
    val m = sc.nextInt()
    val coins = new Array[Int](m)
    for (i <- 0 until m) {
      coins(i) = sc.nextInt()
    }
    println(change(n, coins))
  }
}
