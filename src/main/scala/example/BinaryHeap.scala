package example

/**
  *
  */
class BinaryHeap {


  def insert(node: Node): BinaryHeap = ???

  def siftDown(array: Array[Node], i: Int, lastLeaf: Int): Array[Node] = {
    var max = i
    val left = leftChild(i)
    val right = rightChild(i)
    if( left <= lastLeaf && array(left).value > array(max).value)
      max = left
    if( right <= lastLeaf && array(right).value > array(max).value)
      max = right
    if(i != max) {
      swap(array, max, i)
      siftDown(array, max, lastLeaf)
    }
    array
  }

  private def swap(array: Array[Node], i: Int, j: Int) = {
    val hold = array(i)
    array(i) = array(j)
    array(j) = hold
  }

  def sort(a: Array[Node]): Array[Node] = {
    var m = a.length - 1
    buildHeap(a, m)
    while (m >= 1) {
      swap(a, 0, m)
      m-=1
      siftDown(a, 0, m)
    }
    a
  }

  def buildHeap(array: Array[Node], n: Int) = {
    for( i <- n/2 to 0 by -1){
      siftDown(array, i, n)
    }

  }

  private def leftChild(i: Int) = 2*i
  private def rightChild(i: Int) = 2*i + 1
  private def parent(i: Int) = i / 2
}


case class Node(value: Int)