package interview_cake

/**
 * Inspired by: https://www.interviewcake.com/question/java/matching-parens
 *
 * "Sometimes (when I nest them (my parentheticals) too much (like this (and this))) they get confusing."
 * Write a function that, given a sentence like the one above, along with the position of an opening
 * parenthesis, finds the corresponding closing parenthesis.
 * Example: if the example string above is input with the number 10 (position of the first parenthesis),
 * the output should be 79 (position of the last parenthesis).
 */
object Parentheticals {

  def parentheticalsWithStack(sentence: String, index: Int): Int = {
    var i = 0
    var x = 0
    var stack = List.empty[(Int, Char)]
    while (x == 0 || i != sentence.length) {
      if (sentence(i) == '(') stack = (i, '(') :: stack
      else {
        if (sentence(i) == ')') {
          if (stack.head._1 == index) x = i
          else stack = stack.tail
        }
      }
      i += 1
    }
    x
  }

  def parentheticals(sentence: String, index: Int): Int = {
    var i = index
    var counter = 0
    while (i != sentence.length) {
      if (sentence(i) == '(') counter += 1
      if (sentence(i) == ')') counter -= 1
      if (counter == 0) return i
      i += 1
    }
    -1
  }

  def main(args: Array[String]) {
    val sentence = "Sometimes (when I nest them (my parentheticals) too much " +
      "(like this (and this))) they get confusing."
    println(parentheticals(sentence, 10))
  }
}