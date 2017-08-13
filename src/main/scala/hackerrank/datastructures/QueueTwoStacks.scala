package hackerrank.datastructures

/**
 * https://www.hackerrank.com/challenges/queue-using-two-stacks
 */
object QueueTwoStacks {

  val queue = new Queue {}

  def run(b: Seq[Array[String]]): Unit = {
    b.foreach { seq =>
      if (seq.nonEmpty) {
        seq(0) match {
          case "1" =>
            queue.enqueue(seq(1).toInt)
          case "2" =>
            queue.dequeue
          case "3" =>
            println(queue.head)
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

trait Queue {
  var push: List[Int] = Nil
  var pop: List[Int] = Nil

  def enqueue(int: Int): Unit = {
    push = int :: push
  }
  def dequeue: Int = {
    pop match {
      case Nil =>
        reverseStacks
        val head = pop.head
        pop = pop.tail
        head
      case head :: tail =>
        pop = tail
        head
    }
  }

  private def reverseStacks = {
    val hold = push.reverse
    push = Nil
    pop = hold
  }

  def head: Int = pop.headOption match {
    case None =>
      reverseStacks
      pop.head
    case Some(x) => x
  }
}