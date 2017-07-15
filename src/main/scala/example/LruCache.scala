package example

import scala.collection.mutable

/**
  *
  */
trait LruCache[K, V] {
  def +(key: K, value: V)
  def get(key: K): Option[V]
}


class LruCache1[K: Manifest, V](val capacity: Int) extends LruCache[K, V]{

  private val bucket = new HashMap[K, V]
  private val pages = new MutableDLL[K]

  override def +(key: K, value: V){
    if(pages.size > capacity) {
      val lru = pages.last
      bucket - lru
    }
    bucket + (key -> value)
    pages += key
  }

  override def get(key: K): Option[V] = {
    val i = pages.indexOf(key)
    pages.remove(i)
    pages += key
    pages dropRight 1
    bucket.get(key)
  }
}

/**
  * Step thru tutorial by Mark Lewis
  *
  * https://www.youtube.com/watch?v=ZPz0S9XGG6o
  * @tparam A
  */
class MutableDLL[A: Manifest] extends mutable.Buffer[A]{

  private class Node(var data: A, var prev: Node, var next: Node)
  private val end = new Node(new Array[A](1)(0), null, null)

  end.prev = end
  end.next = end
  private var numElems = 0

  override def apply(n: Int): A = {
    if(n < 0 || n >= numElems) throw new IndexOutOfBoundsException("Requested " + n + " of dll does not exists.")
    var rover = end.next
    for( i <- 0 until n) rover = rover.next
    rover.data
  }

  override def update(n: Int, newelem: A): Unit = {
    if(n < 0 || n >= numElems) throw new IndexOutOfBoundsException("Updating " + n + " of dll does not exists.")
    var rover = end.next
    for( i <- 0 until n) rover = rover.next
    rover.data = newelem

  }

  override def clear(): Unit = {
    end.prev = end
    end.next = end
    numElems = 0
  }

  override def length: Int = numElems

  override def remove(n: Int): A = {
    if(n < 0 || n >= numElems) throw new IndexOutOfBoundsException("Updating " + n + " of dll does not exists.")
    numElems -= 1
    var rover = end.next
    for( i <- 0 until n) rover = rover.next
    val ret = rover.data
    rover.next.prev = rover.prev
    ret
  }

  override def +=:(elem: A) = {
    val n = new Node(elem, end, end.next)
    end.next.prev = n
    end.next = n
    numElems += 1
    this
  }

  override def +=(elem: A) = {
    val n = new Node(elem, end.prev, end)
    end.prev.next = n
    end.prev = n
    numElems += 1
    this
  }

  override def insertAll(n: Int, elems: Traversable[A]): Unit = {
    var rover = end.next
    for( i <- 0 until n) rover = rover.next
    for(e <- elems) {
      val n = new Node(e, rover.prev, rover)
      numElems += 1
      rover.prev.next = n
      rover.prev = n
    }
  }

  override def iterator: Iterator[A] = new Iterator[A] {

    override def hasNext: Boolean = rover != end

    override def next(): A = {
      val ret = rover.data
      rover = rover.next
      ret
    }

    var rover = end.next
  }
}