package hackerrank.datastructures

/**
 * https://www.hackerrank.com/challenges/maximum-element
 */
object MaximumElement {

  var max = -1
  def run(b: Seq[Array[String]]): Unit = {
    b.foldLeft(List.empty[Int]) { (acc, seq) =>
      if (seq.nonEmpty) {
        seq(0) match {
          case "1" =>
            max = Math.max(seq(1).toInt, max)
            seq(1).toInt :: acc
          case "2" => if (acc.nonEmpty) {
            val tail = acc.tail
            if (acc.head == max && tail.nonEmpty) max = acc.tail.max
            else if (tail.isEmpty) max = -1
            acc.tail
          } else Nil
          case "3" =>
            println(max)
            acc
        }
      } else throw new IllegalStateException("This is an illegal state.")
    }
  }

  def main(args: Array[String]): Unit = {
    val sc = new java.util.Scanner(System.in)
    val num = sc.nextLine().toInt
    val instructions = for (_ <- 0 until num) yield sc.nextLine().split(" ")
    run(instructions)
  }
}
