//package example
//
//import scala.annotation.tailrec
//import scala.collection.mutable
//import scala.util.Try
//
//sealed abstract class LinkedListNode[+T]{
//  def isEmpty: Boolean
//  def head: T
//  def tail: LinkedListNode[T]
//  def size: Int
//
//}
//
//
//case class ListNode[T]( val value: T,  val tail: ListNode[T] = null) extends LinkedListNode[T] {
//  def this(value: T) = this(value, null)
//  override def isEmpty: Boolean = (value != null && tail != null)
//  override def head: T = value
//  def ::(value: T) = new ListNode(value, this)
//  def :::(xs: ListNode[T]) = new ListNode[T](this.value, xs)
//  def length: Int = {
//    // Create an empty queue for level order tarversal
//    val q = new mutable.Queue[ListNode[T]]()
//
//    // Enqueue Root and initialize length
//    q.enqueue(this)
//    var length = 0
//
//    @tailrec
//    def breadthSearch(queue: Queue[ListNode[T]]): Int = {
//      val nodeCount = queue.size
//      if (nodeCount == 0) length
//      else{
//        (nodeCount to 1 by -1).foreach{ x =>
//          val node = queue.dequeue()
//          if(node.tail != null)
//            queue.enqueue(node.tail)
//          length = length + 1
//        }
//        breadthSearch(queue)
//      }
//    }
//    breadthSearch(q)
//  }
//
//  def takeRight(n: Int) : ListNode[T] =
//    Try{ (length to (length - n) by -1).foldLeft(this){(x, y) => x.tail} } getOrElse{
//      throw new IllegalStateException(s"The ${n}th position is longer than the list.")
//    }
//
//  def takeRightR(n: Int): ListNode[T] = {
//    val kThFromRight = length - n
//
//    @tailrec
//    def doTake(node: ListNode[T], take: Int): ListNode[T] = {
//      if(node == null) throw new IllegalStateException(s"The ${n}th position is longer than the list.")
//      else if(take == kThFromRight) node
//      else doTake(node.tail, take + 1)
//    }
//    doTake(this, 0)
//  }
//
//  def deleteNode(node: ListNode[T]): ListNode[T] = {
//    def doDelete(linkList:ListNode[T]): ListNode[T] ={
//      if(linkList.value == node.value) {
//        if(linkList.tail != null) doDelete(linkList.tail)
//        else linkList
//      }
//      else this.copy(linkList.value, if(linkList.tail != null) doDelete(linkList.tail) else linkList)
//    }
//
//    doDelete(this)
//  }
//}