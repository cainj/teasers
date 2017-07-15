package example.structures

import scala.annotation.tailrec

/**
  * Trait for a Binary Search Tree
  */
sealed abstract class Tree[A](var key: A, var left: Tree[A], var right: Tree[A]) {


//  /**
//    * Check to see if the tree is empty
//    *
//    * @return
//    */
//  def isEmpty: Boolean = height == 0
//
//  /**
//    * Check to see if the tree is balanced.
//    *
//    * @return
//    */
//  def isBalanced: Boolean = (height - minDepth) < 1

  @tailrec
  private def foldLoop[A, B](a: List[Tree[A]], z: B)(f: (B, A) => B)(o: (Tree[A], List[Tree[A]]) => List[Tree[A]]): B =  a match {
    case head :: tl if(head != null) => println(head.key); foldLoop(o(head, tl), z)(f)(o)
    case head :: tl if(head != null && head.left == null && head.right == null) => foldLoop(tl, f(z, head.key))(f)(o)
    case (e: Eval[A]) :: tl => foldLoop(tl, f(z, e.k))(f)(o)
    case _ => z
  }

  /**
    * fold with inorder traversal (left, root, right)
    * tail recursive optimized
    *
    *        F
    *      /   \
    *    B       G
    *   / \       \
    *  A   D       I
    *     / \     /
    *    C   E   H
    *
    * head evaluate accumulator
    * ---- -------- -----------
    *              | (F)
    * F   | ()     | B::F::G::()
    * B   | ()     | A::B::D::(F,G)
    * A   | (A)    | (B,D,F,G)
    * B   | (B)    | (D,F,G)
    * D   | ()     | C::D::E::(F,G)
    * C   | (C)    | (D,E,F,G)
    * D   | (D)    | (E,F,G)
    * E   | (E)    | (F,G)
    * F   | (F)    | (G)
    * G   | ()     | G::I::()
    * G   | (G)    | (I)
    * I   | ()     | H::I::()
    * H   | (H)    | H
    * I   | (I)    | ()
    *
    * result
    * A,B,C,D,E,F,G,H,I
    */
  def foldInorder[B](z: B)(f: (B, A) => B): B = {
    foldLoop(List(this), z)(f) { (n, tl) => n.left :: Eval(n.key) :: n.right :: tl }
  }

  def foldPreorder[B](z: B)(f: (B, A) => B): B = {
    foldLoop(List(this), z) (f) { (n, tl) => Eval(n.key) :: n.left :: n.right :: tl}
  }

  def toList: List[A] = foldInorder(List.empty[A]) {_.:+(_)}


  /**
    * Inserts a new value into the current tree
    *
    * @param key The key value
    * @tparam A The key `type`
    * @return
    */
  def insert[A](key: A): Tree[A] = {
    ???
  }

//
//  /**
//    * Calculates the number of edges for the tree.
//    *
//    * @return
//    */
//  def height: Int = {
//    def loop(t: Tree[A]): Int =  t match {
//      case n: Node[A] => Math.max(loop(n.l) + 1, loop(n.r) + 1)
//      case l: Leaf[A] => 1
//      case _ => 0
//    }
//    loop(this) - 1
//  }
//
//  /**
//    * Finds the closest Tree Node with the given key
//    *
//    * @param key The key
//    * @param ord The ordering that should take place
//    * @tparam A1 the type of key `type`
//    * @return
//    */
//  def find[A1 >: A](key: A1)(implicit ord: Ordering[A1]): Option[Tree[A]] = {
//
//    def doFind(tree: Tree[A], value: A1): Tree[A] = tree match {
//      case Empty => tree
//      case t: Tree[A] if(t.key.get == value) => tree
//      case _ =>
//        if(ord.lt(value, tree.key.getOrElse(value))) doFind(tree.left.getOrElse(Empty), value)
//        else doFind(tree.right.getOrElse(Empty), value)
//
//    }
//    doFind(this, key) match {
//      case Empty => None
//      case x => Some(x)
//    }
//  }
//
//  /**
//    * Finds the closest Tree Node with the given key
//    *
//    * @param key The key
//    * @param ord The ordering that should take place
//    * @tparam A1 the type of key `type`
//    * @return
//    */
//  def findNearest[A1 >: A](key: A1)(implicit ord: Ordering[A1]): Tree[A1] = {
//
//    def doFind(tree: Tree[A], value: A1, parent: Tree[A]): Tree[A] = tree match {
//      case Empty => parent
//      case t: Tree[A] if(t.key.get == value) => tree
//      case _ =>
//        if(ord.lt(value, tree.key.getOrElse(value))) doFind(tree.left.getOrElse(Empty), value, tree)
//        else doFind(tree.right.getOrElse(Empty), value, tree)
//
//    }
//    doFind(this, key, Empty)
//  }
//
//  def minDepth: Int = {
//    def loop(t: Tree[A]): Int = t match {
//      case l: Leaf[A] => 1
//      case n: Node[A] => Math.min(loop(n.l) + 1, loop(n.l) + 1)
//      case _ => -1
//    }
//    loop(this)
//  }
//
//  /**
//    * Finds the min key for the tree
//    *
//    * @tparam A1
//    * @return
//    */
//  def min[A1 >: A]: A = minTree.key.get
//
//  /**
//    * Finds the min node for a tree
//    * @return
//    */
//  def minTree: Tree[A] = {
//    if (isEmpty)
//      throw new UnsupportedOperationException("empty.min")
//    def doMin(tree: Tree[A], min: Tree[A]): Tree[A] = tree match {
//      case Empty => min
//      case l: Leaf[A] => l
//      case _ => doMin(tree.left.getOrElse(Empty), min)
//    }
//    doMin(this, this)
//  }
//
//  /**
//    * Finds the max node of the tree.
//    *
//    * @return
//    */
//  def maxTree: Tree[A] = {
//    if (isEmpty)
//      throw new UnsupportedOperationException("empty.max")
//    def doMax(tree: Tree[A], max: Tree[A]): Tree[A] = tree match {
//      case Empty => max
//      case l: Leaf[A] => l
//      case _ => doMax(tree.right.getOrElse(Empty), max)
//    }
//    doMax(this, this)
//  }
//
//  /**
//    * Finds the max value of a tree.
//    *
//    * @return
//    */
//  def max: A = maxTree.key

  case class Eval[A](k: A) extends Tree[A](k, null, null)
}

class Node[A](k: A, l: Tree[A] = null, r: Tree[A] = null) extends Tree[A](k, l, r)


object Tree {

  /**
    * Creates a Tree node
    *
    * @param key
    * @param l Left child
    * @param r Right child
    * @tparam A The key `type`
    * @return
    */
  def apply[A](key: A, l: Tree[A], r: Tree[A]): Tree[A] = new Node(key, l, r)

  /**
    * Creates a full binary search tree with the given elements
    *
    * @param keys The keys to put in a binary search tree
    * @tparam A The key `type`
    * @return
    */
  def apply[A](keys: A*): Tree[A] = ???


}

