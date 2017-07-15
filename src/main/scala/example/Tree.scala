package example

trait Tree[+A]{

  def left: Tree[A]

  def key: A

  def right: Tree[A]

  def isEmpty: Boolean = false

  def isBalanced: Boolean = height - minDepth < 2

  def height: Int = {
    def findHeight(node: Tree[A]): Int = {
      if(node == EmptyTree) -1
      else 1 + Math.max(findHeight(node.left), findHeight(node.right))
    }
    findHeight(this)
  }

  def minDepth: Int = {
    def findHeight(node: Tree[A]): Int = {
      if(node == EmptyTree) -1
      else 1 + Math.min(findHeight(node.left), findHeight(node.right))
    }
    findHeight(this)
  }

  def min[A1 >: A]: A = {
    if (isEmpty)
      throw new UnsupportedOperationException("empty.min")
    def doMin(tree: Tree[A], min: A): A ={
      if(tree.left == EmptyTree) min
      else doMin(tree.left, tree.left.key)
    }
    doMin(this, this.key)
  }

  def search[A1 >: A](key: A1)(implicit ord: Ordering[A1]): Tree[A1] = {

    def doSearch(tree: Tree[A1], value: A1): Tree[A1] = {
      if(tree == EmptyTree || tree.key == value) tree
      else if(ord.lt(value, tree.key)) doSearch(tree.left, value)
      else doSearch(tree.right, value)

    }
    doSearch(this, key)
  }

  def max[A1 >: A]: A = {
    if (isEmpty)
      throw new UnsupportedOperationException("empty.max")
    def doMax(tree: Tree[A], max: A): A = {
      if(tree.right == EmptyTree) max
      else doMax(tree.right, tree.right.key)
    }
    doMax(this, this.key)
  }

  /**
    * Adds a new Tree with the given key
    *
    * @param key The key to add to the tree
    * @tparam A The key type
    * @return
    */
  def +[A](key: A): Tree[A] = ???

  /**
    *
    * @param tree
    * @tparam A
    * @return
    */
  def ++[A](tree: Tree[A]): Tree[A] = ???

}

/**
  * Empty BinarySearchTree
  */
case object EmptyTree extends Tree[Nothing] {
  override def isEmpty: Boolean = true
  override def isBalanced: Boolean = false
  override def height: Int = throw new UnsupportedOperationException("No height for empty tree")
  override def left: Tree[Nothing] = throw new NoSuchElementException("No such element for empty tree")
  override def key: Nothing = throw new NoSuchElementException("No such element for empty tree")
  override def right: Tree[Nothing] = throw new NoSuchElementException("No such element for empty tree")
}


case class BinarySearchTree1[A](var key: A, var left: Tree[A] = EmptyTree, var right: Tree[A] = EmptyTree)
  extends Tree[A]{

  def inOrder: List[A] = {
    var list = List.empty[A]
    def doInOrder(tree: Tree[A]): List[A] = {
      tree match {
          case EmptyTree => list
          case BinarySearchTree1(x, EmptyTree, r) =>
            list = list :+ x
            doInOrder(r)
          case BinarySearchTree1( x, l, EmptyTree) =>
            doInOrder(l)
          case BinarySearchTree1(x, l, r) =>
            doInOrder(l)
            list =  list :+ x
            doInOrder(r)
        }
      }
    doInOrder(this)

  }
}