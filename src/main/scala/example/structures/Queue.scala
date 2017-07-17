package example.structures

/**
 *
 */
class Queue[K] extends QueueLike[K] {

  private var enqueueStack = List.empty[K]
  private var dequeueStack = List.empty[K]

  override def enqueue(value: K) {
    enqueueStack = value :: enqueueStack
  }

  override def dequeue: K = {
    if (dequeueStack.isEmpty) {
      dequeueStack = enqueueStack.reverse
      enqueueStack = Nil
    }
    val head = dequeueStack.headOption match {
      case Some(x) =>
        dequeueStack = dequeueStack.tail
        x
      case None => throw new IllegalStateException("No Elements in the queue.")
    }
    head
  }
}

trait QueueLike[K] {
  def enqueue(value: K)
  def dequeue: K
}