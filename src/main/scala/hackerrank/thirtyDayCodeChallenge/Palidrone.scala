package hackerrank.thirtyDayCodeChallenge

import java.util.Scanner

import scala.collection.mutable.{ Queue, Stack }

object Solution {

  var queue = new Queue[Char]()
  var stack = new Stack[Char]()

  def palidrone(s: String, acc: List[Char]): List[Char] = {
    if (s.isEmpty) acc
    else palidrone(s.tail, s.head :: acc)
  }

  def pushCharacter(c: Char) { stack.push(c) }

  def enqueueCharacter(c: Char) = queue.enqueue(c)

  def dequeueCharacter = queue.dequeue()

  def popCharacter = stack.pop

  def main(args: Array[String]) {
    val sc = new Scanner(System.in)
    val n = sc.next
    val answer = if (n == new String(palidrone(n, Nil).toArray)) s"$n, is" else s"$n is not"
    println(s"The word, $answer a palidrone")
  }
}