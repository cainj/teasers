package example

import scala.annotation.tailrec

/**
    Consider a string, expression consisting of the characters < and > only. We consider the string to be balanced if each < always appears before (i.e., to the left of) a corresponding > character (they do not need to be adjacent). Moreover, each < and > act as a unique pair of symbols and neither symbol can be considered as part of any other pair of symbols. For example, the strings <<>>, <>, and <><> are all balanced, but the strings >>, <<>, and ><>< are unbalanced.

    To balance a string, we can replace any > character with <> at most maxReplacement times. Given an expression and the value of maxReplacement, can you turn an unbalanced string into a balanced one?

    Complete the balancedOrNot function in the editor below. It has the following parameters:

    An array of n strings, expressions, denoting the list of expressions to check.
    An array of n integers, maxReplacements, where maxReplacementsi denotes the maximum number of replacements allowed when attempting to balance expressionsi.

    The function must return an array of integers where each index i (0 ≤ i < n) contains a 1 if expressionsi is balanced or a 0 if it is not.

    Input Format
    The first line contains an integer, n, denoting the size of expressions.
    Each line i of the n subsequent lines (where 0 ≤ i < n) contains a string describing expressionsi.
    The next line contains an integer, m, denoting the size of maxReplacements.
    Each line i of the n subsequent lines (where 0 ≤ i < n) contains a string describing maxReplacementsi.

    Constraints
    1 ≤ n ≤ 102
    1 ≤ length of expressionsi ≤ 105
    0 ≤ maxReplacementsi ≤ 105

    Output Format
    The function must return an array of integers where each index i (0 ≤ i < n) contains a 1 if expressions i is balanced or a 0 if it is not.

    Sample
    (["<>", "<>><"] , [2,1]) return [1,0]

  */
object Balancer {

  def balancedOrNot(expressions: Array[String], maxReplacements: Array[Int]): Array[Int] = {
    val x = (for( i <- expressions.indices; str = expressions(i)) yield (findBalance(str, Nil), maxReplacements(i))).toArray
    x map { case (chars, max) => if( (chars.nonEmpty && chars.last == '<') || chars.size > max) 0 else 1 }
  }

  @tailrec
  def findBalance(s: String, xs: List[Char]): List[Char] = {
    if(s == "") xs
    else{
      val c = s.head
      if( c == '>' && xs != Nil && xs.head == '<') findBalance(s.tail, xs.tail)
      else findBalance( s.tail, c :: xs)
    }

  }


  def main(args: Array[String]) {
    println(balancedOrNot(Array("<>", "<>><"), Array(2,1)).toList == List(1,0))
  }
}
