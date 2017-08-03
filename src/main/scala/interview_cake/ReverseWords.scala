package interview_cake

/**
 * Inspired by: https://www.interviewcake.com/question/java/reverse-words
 *
 * Your team is scrambling to decipher a recent message, worried it's a plot
 * to break into a major European National Cake Vault. The message has been mostly deciphered,
 * but all the words are backwards! Your colleagues have handed off the last step to you.
 * Write a function reverseWords() that takes a string message and reverses the order of t
 * he words in-place.
 */
object ReverseWords {

  def reverseWords(n: Array[String]): String = {
    val reverse = for {
      i <- 0 until n.length / 2
    } yield swap(n, i, n.length - 1 - i)
    n mkString (" ")
  }

  def swap(xs: Array[String], i: Int, j: Int) = {
    println(s"$i $j")
    val hold = xs(j)
    xs(j) = xs(i)
    xs(i) = hold
  }

  def main(args: Array[String]) {
    val message = "find you will pain only go you recordings security the into if"
    println(reverseWords(message split (" ")))
  }
}
