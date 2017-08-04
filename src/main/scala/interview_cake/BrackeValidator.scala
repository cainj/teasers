package interview_cake

/**
 *
 */
object BrackeValidator {

  def validate(n: String): Boolean = {
    var stack = n.toList
    ???
  }

  def main(args: Array[String]) {
    val sc = new java.util.Scanner(System.in)
    val check = sc.next()
    println(validate(check))
  }
}
