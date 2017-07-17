package example.structures

/**
  * The trait for a priority queue.
  * https://en.wikipedia.org/wiki/Priority_queue
  */
trait PriorityQueue[A] {

  /**
    * Puts an element on the queue
    *
    * @param a
    */
  def enqueue(a: A)

  /**
    * Pops the first element of the queue
    * @return
    */
  def dequeue(): A

  /**
    * First element in the queue
    *
    * @return
    */
  def peek: A

  /**
    * Tell if the Queue is empty
    * @return
    */
  def isEmpty: Boolean

}
