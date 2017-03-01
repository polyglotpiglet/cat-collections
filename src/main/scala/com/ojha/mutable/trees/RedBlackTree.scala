package com.ojha.mutable.trees

import com.ojha.mutable.ArrayBackedTree
import com.ojha.mutable.graphs.Node

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer

/**
  *
  * A red black tree is a beautiful thing.
  *
  * The laws of red black trees are the following:
  *
  * - Every node is red or black
  * - The root must be black
  * - Red nodes mustn't have red children (which also means no red can have a red parent)
  * - The paths from the root to any Nil node must all contain the same number of black nodes
  *
  * Eg
  *
  * Black
  * Red    Red
  *
  * ^ valid red black tree ^
  *
  * Eg
  *
  * Red
  * Black Black
  *
  * ^ invalid red black tree because the root isn't black ^
  *
  * Eg
  *
  * Black
  * Red   Red
  * Red
  *
  * ^ invalid red black tree because there is a red - red parent child relationship ^
  *
  * Eg
  *
  * Black
  * Red    Red
  * Black
  *
  * ^ invalid because Root -> Left -> Left -> Null contains two blacks but other routes to null contain only one ^
  *
  */

object RedBlackTree {
  def empty[T](implicit ord: Ordering[T]) = new RedBlackTree[T]()

  def of[T](ts: T*)(implicit ord: Ordering[T]): RedBlackTree[T] = {
    val tree = new RedBlackTree[T]()
    ts.foreach(tree.insert)
    tree
  }

}

abstract class Node[T](val value: T) {
  var left: Option[Node[T]] = None
  var right: Option[Node[T]] = None
}

case class Red[T](override val value: T) extends Node[T](value)
case class Black[T](override val value: T) extends Node[T](value)



/**
  * @param ord
  * @tparam T
  */
class RedBlackTree[T](implicit ord: Ordering[T]) extends Tree[T] {

  private var root: Option[Node[T]] = None

  private var currentSize = 0
  override def size: Int = currentSize

  override def nonEmpty: Boolean = root.isDefined

  override def isEmpty: Boolean = root.isEmpty

  /**
    * If we are inserting the root then make it black (one of the red black tree invariants is that the root must be black)
    * If we are inserting an element other than the root then it gets a little more interesting... (see comments for nonRootInsert)
    * @param value
    */
  def insert(value: T): Unit = root match {
    case None => root = Some(Black(value)); currentSize += 1
    case Some(_) => nonRootInsert(value, root, None) ; currentSize += 1
  }

  /**
    *
    * We try to make the newly inserted element Red. If it's parent is black then WHOOP we are done, because all the invariants
    * remain unbroken.
    *
    * However if the parent of the newly insert red node is also red then we have a little problem becauase we are not meant to have
    * double red nodes in our tree.
    *
    * @param value
    * @param current
    * @param parent
    */
  private def nonRootInsert(value: T, current: Option[Node[T]], parent: Option[Node[T]]): Unit = (current, parent) match {
    case (None, Some(p)) if ord.gt(p.value, value) => p.left = Some(Red(value))
    case (None, Some(p)) if ord.lt(p.value, value) => p.right = Some(Red(value))
    case (Some(node), _) if ord.gt(node.value, value) => nonRootInsert(value, node.left, current)
    case (Some(node), _) if ord.lt(node.value, value) => nonRootInsert(value, node.right, current)
  }

  def inOrder: Seq[T] = inOrderTraversalFrom(root)

  private def inOrderTraversalFrom(node: Option[Node[T]], result: List[T] = List.empty): List[T] = node match {
    case Some(n) => (inOrderTraversalFrom(n.left) :+ n.value) ++ inOrderTraversalFrom(n.right)
    case None => result
  }
}
