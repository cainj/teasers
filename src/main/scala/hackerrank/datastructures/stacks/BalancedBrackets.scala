package hackerrank.datastructures.stacks

object BalancedBrackets {

  val brackets = Map(')' -> '(', ']' -> '[', '}' -> '{')
  def validateBrackets(b: String): Boolean = {
    var s = b
    var stack = List.empty[Char]
    var flag = true
    while (s.nonEmpty && flag) {
      if (brackets.valuesIterator.contains(s.head))
        stack = s.head :: stack
      else {
        brackets.get(s.head) match {
          case Some(x) if (stack.headOption.getOrElse(' ') == x) => stack = stack.tail
          case _ => flag = false
        }
      }
      s = s.tail
    }
    stack.isEmpty && flag
  }

  def main(args: Array[String]): Unit = {
    val sc = new java.util.Scanner(System.in)
    val num = sc.nextLine().toInt
    val listOfBrackets = for (_ <- 0 until num) yield sc.nextLine()
    listOfBrackets map { validateBrackets } foreach {
      if (_) println("YES") else println("NO")
    }
  }
}
