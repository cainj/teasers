package example.structures

import java.util.NoSuchElementException

/**
 *
 */
class HeapPriorityQueue[A: Manifest](higherPriority: (A, A) => Boolean) extends PriorityQueue[A] {

  var heap = new Array[A](10)
  var back = 1

  /**
   * Puts an element on the queue
   *
   * @param o The object to enqueue
   */
  override def enqueue(o: A): Unit = {
    if (back >= heap.length) {
      val tmp = new Array[A](heap.length * 2)
      for (i <- 1 until heap.length) tmp(i) = heap(i)
      heap = tmp
    }
    var bubble = back
    while (bubble > 1 && higherPriority(o, heap(bubble / 2))) {
      heap(bubble) = heap(bubble / 2)
      bubble /= 2
    }
    heap(bubble) = o
    back += 1
  }

  /**
   * First element in the queue
   *
   * @return
   */
  override def peek: A = if (isEmpty) throw new NoSuchElementException("empty.peek") else heap(1)

  /**
   * Pops the first element of the queue
   * @return
   */
  override def dequeue(): A = {
    if (isEmpty) throw new NoSuchElementException("empty.dequeue")
    else {
      val ret = heap(1)
      back -= 1
      val tmp = heap(back)
      var stone = 1
      var flag = true
      while (stone * 2 < back && flag) {
        var priorityChild = stone * 2
        if (stone * 2 + 1 < back && higherPriority(heap(stone * 2 + 1), heap(stone * 2))) priorityChild += 1
        if (higherPriority(heap(priorityChild), tmp)) {
          heap(stone) = heap(priorityChild)
          stone = priorityChild
        } else
          flag = false
      }
      heap(stone) = tmp
      ret
    }
  }

  /**
   * Tell if the Queue is empty
   * @return
   */
  override def isEmpty: Boolean = back == 1
}
