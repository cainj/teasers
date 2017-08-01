package example

/**
 *
 */
object KaratSuba {

  def karatSuba(x: String, y: String): Double = {
    if (x.length == 1 && y.length == 1) x.toInt * y.toInt
    else {
      val a = x.substring(0, x.length / 2)
      val b = x.substring(x.length / 2, x.length)
      val c = y.substring(0, y.length / 2)
      val d = y.substring(y.length / 2, y.length)
      (Math.pow(10, x.length) * karatSuba(a, c)) +
        (Math.pow(10, x.length / 2) * (karatSuba(a, d) + karatSuba(b, c))) +
        karatSuba(b, d)
    }
  }

  def main(args: Array[String]) {
    val sc = new java.util.Scanner(System.in)
    val x = sc.next
    val y = sc.next
    val ans = karatSuba(x, y)
    println(ans)

  }
}
