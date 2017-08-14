package hackerrank.datastructures.arrays

import scala.collection.mutable.ArrayBuffer

object DynamicArray {

  def run(instructions: Array[Array[Int]], xs: Array[ArrayBuffer[Int]]): Unit = {
    var lastAnswer = 0
    val n = xs.length
    for {
      instruction <- instructions
    } {
      instruction.head match {
        case 1 =>
          val i = (instruction(1) ^ lastAnswer) % n
          val seq = xs(i)
          seq += instruction(2)

        case 2 =>
          val i = (instruction(1) ^ lastAnswer) % n
          val seq = xs(i)
          val find = instruction(2) % seq.size
          lastAnswer = seq(find) // could do xs.last
          println(lastAnswer)
      }
    }
  }

  def main(args: Array[String]) {
    val sc = new java.util.Scanner(System.in);
    val line = sc.nextLine().split(" ");
    val n = line(0).toInt
    val queries = line(1).toInt
    val arr = new Array[ArrayBuffer[Int]](n);
    for (i <- 0 until n) {
      val xs = ArrayBuffer[Int]()
      arr(i) = xs
    }
    val instructions = new Array[Array[Int]](queries);
    for (i <- 0 until queries) { instructions(i) = sc.nextLine.split(" ") map { _.toInt } }
    run(instructions, arr)
  }
}
