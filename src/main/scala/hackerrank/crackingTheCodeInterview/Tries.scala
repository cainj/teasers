package hackerrank.crackingTheCodeInterview

import scala.collection.mutable

/**
  * https://www.hackerrank.com/challenges/ctci-contacts/problem
  */
object Tries {

  def myPrint(node: Node): Unit = {
    if (node.children.nonEmpty) {
      println(node.children + " " + node.isCompletedWord)
      node.children.foreach(b => myPrint(b._2))
    }
  }

  def main(args: Array[String]) {
    val sc = new java.util.Scanner(System.in)
    val n = sc.nextInt()
    var a0 = 0
    val node = new Node()
    while (a0 < n) {
      var op = sc.next()
      var contact = sc.next()
      op match {
        case "add" => node.add(contact)
        case "find" => println(node.find(contact) + " ")
      }
      a0 += 1
    }
  }

  class Node(val isCompletedWord: Boolean = false) {
    val children = mutable.HashMap.empty[Char, Node]
    var count = 0

    def add(contact: String): Unit = {
      var i = 0
      val lastChar = contact.length - 1
      var isLastChar = isCompletedWord
      var currentNode = this
      while (i < contact.length) {
        val c = contact(i)
        currentNode.count += 1
        if (lastChar == i) isLastChar = true
        val data = currentNode.children.getOrElse(c, new Node(isLastChar))
        currentNode.children += (c -> data)
        currentNode = data
        i += 1
      }
      currentNode.count += 1
    }

    def find(query: String): Int = {
      def findUtil(contact: String, node: Node): Int = {
        if (contact.length == 1 && node.children.get(contact.head).isDefined) node.children(contact.head).count
        else if (node.children.get(contact.head).isEmpty) 0
        else findUtil(contact.tail, node.children(contact.head))
      }
      findUtil(query, this)
    }
  }
}