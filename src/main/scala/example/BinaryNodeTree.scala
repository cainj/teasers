package example

import scala.annotation.tailrec
import scala.collection.mutable


case class BinaryTreeNode( value: Int, left: BinaryTreeNode = null, val right: BinaryTreeNode = null) {

  def insertLeft(leftValue: Int): BinaryTreeNode =
   copy(this.value, BinaryTreeNode(leftValue), this.right)

  def insertRight(rightValue: Int) =
    copy(this.value, this.left, BinaryTreeNode(rightValue))

  def height: Int = {
    def findHeight(node: BinaryTreeNode): Int = {
      if(node == null) -1
      else{
        1 + Math.min(findHeight(node.left), findHeight(node.right))
      }
    }
    findHeight(this) - 1
  }

  def isBalanced: Boolean =
    (height - shortestDepthR) <= 1

  def longestDepthR: Int = {
    def findDepth(node: BinaryTreeNode): Int = {
      if(node == null) -1
      else {
        val lHeight = findDepth(node.left)
        val rHeight = findDepth(node.right)
        if(lHeight >= rHeight) lHeight + 1
        else rHeight + 1
      }
    }
    findDepth(this)
  }

  def search(value: Int) = {
    def doSearch(value: Int, tree: BinaryTreeNode):BinaryTreeNode = {
      if(tree == null || tree.value == value) tree
      else if(value < tree.value) doSearch(value, tree.left)
      else doSearch(value, tree.right)
    }
    doSearch(value, this)
  }

  def longestDepth: Int = {

    // Create an empty queue for level order tarversal
    val q = new mutable.Queue[BinaryTreeNode]()

    // Enqueue Root and initialize height
    q.enqueue(this)
    var height = -1

    while (true)
    {
      // nodeCount (queue size) indicates number of nodes
      // at current level.
      var nodeCount = q.size
      if (nodeCount == 0)
        return height
      height = height + 1

      // Dequeue all nodes of current level and Enqueue all
      // nodes of next level
      nodeCount = (nodeCount to 0).fold(0){(x, y) =>
         val newnode = q.dequeue()
         if (newnode.left != null)
           q.enqueue(newnode.left)
         if (newnode.right != null)
           q.enqueue(newnode.right)
         x - y
       }
    }
    height
  }

  def shortestDepthR: Int = {
    def findDepth(node: BinaryTreeNode): Int = {
      if(node == null) -1
      else {
        val lHeight = findDepth(node.left)
        val rHeight = findDepth(node.right)
        if(lHeight <= rHeight) lHeight + 1
        else rHeight + 1
      }
    }
    findDepth(this)
  }

  def findClosestValue(value: Int): Int = {

    @tailrec
    def doClosest( node: BinaryTreeNode, closestValue: Int): Int = {
      if(node == null) closestValue
      else if(node.value == value) node.value
      else {
        val direction = if (node.value > value) node.left else node.right
        if (Math.abs(value - closestValue) > Math.abs(node.value - value)) doClosest(direction, node.value)
        else doClosest(direction, closestValue)
      }
    }

    doClosest(this, this.value)
  }


  /**
    * Runs in O(h) time
    * @return
    */
  def min: BinaryTreeNode = {
    var x = this
    var min = this
    while( x != null){
      if(x.left != null)
        min = x.left
      x = x.left
    }
    min
  }

  /**
    * Runs in O(h) time
    * @return
    */
  def max: Int = {

    def doMax(tree: BinaryTreeNode, max: Int): Int ={
      if(tree.right == null) max
      else doMax(tree.right, tree.right.value)
    }
    doMax(this, this.value)
  }

}

