package example.structures

import scala.collection.mutable.ListBuffer
import scala.util.Try

/**
 * Trait for a Binary Search Tree
 */
sealed abstract class Tree[A: Manifest](var key: A, var left: Tree[A], var right: Tree[A]) {

  /**
   * Check to see if the tree is empty
   *
   * @return
   */
  def isEmpty: Boolean = height == 0

  /**
   * Check to see if the tree is balanced.
   *
   * @return
   */
  def isBalanced: Boolean = (height - minDepth) < 1

  /**
   * Inserts a new value into the current tree
   *
   * @param key The key value
   * @tparam _ The key `type`
   * @return
   */
  def insert[_ >: A](key: A)(implicit ord: Ordering[A]): Tree[A] = {
    val leaf = findNearest(key)(ord)
    if (ord.lt(leaf.key, key)) leaf.right = new TreeNode(key)
    else leaf.left = new TreeNode[A](key)
    new TreeNode(this.key, this.left, this.right)
  }

  def removeParent(value: Tree[A], parent: Tree[A])(implicit ord: Ordering[A]) {
    if (parent.left == value) parent.left = null
    else if (parent.right == value) parent.left = null
    else {
      if (ord.lt(value.key, parent.key)) removeParent(value, parent.left)
      else removeParent(value, parent.right)
    }
  }

  /**
   * delete a node from the tree
   *
   * @param key
   * @param ord
   * @tparam _
   */
  def delete[_ >: A](key: A)(implicit ord: Ordering[A]) {
    val n = find(key) getOrElse (throw new NoSuchElementException("No node with that value."))
    if (isLeaf(n)) removeParent(n, this)
    else if (n.right != null) n.key = n.right.minTree.key
    else n.key = n.left.maxTree.key
  }

  /**
   * Calculates the number of edges for the tree.
   *
   * @return
   */
  def height: Int = {
    def loop(t: Tree[A]): Int = {
      if (t == null) 0
      else if (isLeaf(t)) 1
      else if (isParent(t)) Math.max(loop(t.left) + 1, loop(t.right) + 1)
      else 0
    }
    loop(this) - 1
  }

  private def isLeaf(t: Tree[A]) = t.left == null && t.right == null && t.key != null

  private def isParent(t: Tree[A]) = (t.left != null || t.right != null) && t.key != null

  /**
   * Finds the closest Tree Node with the given key
   *
   * @param key The key
   * @param ord The ordering that should take place
   * @tparam A1 the type of key `type`
   * @return
   */
  def find[A1 >: A](key: A1)(implicit ord: Ordering[A1]): Option[Tree[A]] = {

    def doFind(tree: Tree[A], value: A1): Tree[A] = {
      if (tree == null) null
      else if (tree.key == value) tree
      else {
        if (ord.lt(value, tree.key)) doFind(tree.left, value)
        else doFind(tree.right, value)
      }
    }
    doFind(this, key) match {
      case null => None
      case x => Some(x)
    }

  }

  /**
   * Finds the closest Tree Node with the given key
   *
   * @param key The key
   * @param ord The ordering that should take place
   * @tparam A1 the type of key `type`
   * @return
   */
  def findNearest[A1 >: A](key: A1)(implicit ord: Ordering[A1]): Tree[A] = {

    def doFind(tree: Tree[A], value: A1, parent: Tree[A]): Tree[A] = {
      if (tree == null) parent
      else if (tree.key == value) tree
      else {
        if (ord.lt(value, tree.key)) doFind(tree.left, value, tree)
        else doFind(tree.right, value, tree)
      }
    }
    doFind(this, key, null)
  }

  /**
   * Shortest number of edges in the tree
   *
   * @return
   */
  def minDepth: Int = {
    def loop(t: Tree[A]): Int = {
      if (t == null) 0
      if (isLeaf(t)) 1
      else if (isParent(t)) Math.min(loop(t.left) + 1, loop(t.right) + 1)
      else -1
    }
    loop(this)
  }

  /**
   * Finds the min key for the tree
   *
   * @tparam A1
   * @return
   */
  def min[A1 >: A]: A = minTree.key

  /**
   * Finds the min node for a tree
   * @return
   */
  def minTree: Tree[A] = {
    if (isEmpty)
      throw new UnsupportedOperationException("empty.min")
    def doMin(tree: Tree[A], min: Tree[A]): Tree[A] = {
      if (tree == null) min
      else if (isLeaf(tree)) tree
      else doMin(tree.left, min)
    }
    doMin(this, this)
  }

  /**
   * Inspired by https://www.interviewcake.com/question/java/second-largest-item-in-bst
   *
   * @param nTh the nth Number
   */
  def nthLargest(nTh: Int) = {
    val result = new ListBuffer[A]
    /* Given a binary tree, print its nodes in inorder*/
    def doNthLargest(node: Tree[A]) {

      if (node.right != null)
        doNthLargest(node.right)

      result += node.key

      if (node.left != null)
        doNthLargest(node.left)
    }
    doNthLargest(this)
    Try { result(nTh) } getOrElse (throw new IllegalArgumentException(s"Tree is not large enough for the $nTh position."))
  }

  /**
   * Finds the max node of the tree.
   *
   * @return
   */
  def maxTree: Tree[A] = {
    if (isEmpty)
      throw new UnsupportedOperationException("empty.max")
    def doMax(tree: Tree[A]): Tree[A] = {
      if (tree.right != null) doMax(tree.right)
      else tree
    }
    doMax(this)
  }

  def inOrder = {
    val result = new ListBuffer[A]
    /* Given a binary tree, print its nodes in inorder*/
    def doInOrder(node: Tree[A]) {
      if (node.left != null)
        doInOrder(node.left)

      result += node.key

      if (node.right != null)
        doInOrder(node.right)
    }
    doInOrder(this)
    result
  }

  def preOrder = {
    val buffer = new ListBuffer[A]

    var stack = List.empty[Tree[A]]

    stack = this :: stack

    while (stack.nonEmpty) {
      val n = stack.head
      stack = stack.tail
      buffer += n.key

      if (n.right != null) stack = n.right :: stack
      if (n.left != null) stack = n.left :: stack
    }
    buffer
  }

  def postOrder = {
    val buffer = new ListBuffer[A]

    var stack = List.empty[Tree[A]]

    stack = this :: stack

    var prev: Tree[A] = null
    while (stack.nonEmpty) {
      val current = stack.head

      /* go down the tree in search of a leaf an if so process it
      and pop stack otherwise move down */
      if (prev == null || prev.left == current || prev.right == current) {
        if (current.left != null)
          stack = current.left :: stack
        else if (current.right != null)
          stack = current.right :: stack
        else {
          stack = stack.tail
          buffer += current.key
        }

        /* go up the tree from left node, if the child is right
           push it onto stack otherwise process parent and pop
           stack */
      } else if (current.left == prev) {
        if (current.right != null) stack = current.right :: stack
        else {
          stack = stack.tail
          buffer += current.key
        }

        /* go up the tree from right node and after coming back
         from right node process parent and pop stack */
      } else if (current.right == prev) {
        stack = stack.tail
        buffer += current.key
      }
      prev = current
    }
    buffer
  }

  /**
   * Finds the max value of a tree.
   *
   * @return
   */
  def max: A = maxTree.key

}

class TreeNode[A: Manifest](k: A, l: Tree[A] = null, r: Tree[A] = null) extends Tree[A](k, l, r)
