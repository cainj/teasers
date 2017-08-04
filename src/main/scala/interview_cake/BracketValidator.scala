package interview_cake

import scala.util.{ Failure, Success, Try }

/**
 *
 * You're working with an intern that keeps coming to you with JavaScript code
 * that won't run because the braces, brackets, and parentheses are off.
 * To save you both some time, you decide to write a braces/brackets/parentheses validator.
 * Let's say:
 * '(', '{', '[' are called "openers."
 * ')', '}', ']' are called "closers."
 * Write an efficient function that tells us whether or not an input string's openers
 * and closers are properly nested.
 */
object BracketValidator {

  def validate(n: String): Boolean = {
    var s = n
    var stack = List.empty[Bracket]
    Try {
      while (s.nonEmpty) {
        val c = s.head
        if (Bracket.openBracket(c)) {
          stack = Bracket(c) :: stack
        } else if (Bracket.closeBracket(c)) {
          if (stack.head.matches(c)) {
            stack = stack.tail
          } else throw new IllegalStateException("Invalid state")
        }
        s = s.tail
      }
    } match {
      case Success(_) => if (stack.nonEmpty) false else true
      case Failure(_) => false
    }
  }

  def main(args: Array[String]) {
    val sc = new java.util.Scanner(System.in)
    val check = sc.nextLine()
    println(validate(check))
  }

  case class Bracket(open: Char) {
    def matches(close: Char) = close match {
      case ')' if open == '(' => true
      case ']' if open == '[' => true
      case '}' if open == '{' => true
    }
  }

  object Bracket {
    def openBracket(c: Char) = c == '[' || c == '{' || c == '('
    def closeBracket(c: Char) = c == ']' || c == '}' || c == ')'
  }
}
