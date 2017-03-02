package com.ojha.mutable.trees

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

  def withLeft(left: Node[T]): Node[T] = {
    this.left = Some(left)
    this
  }

  def withRight(right: Node[T]): Node[T] = {
    this.right = Some(right)
    this
  }

  def withLeft(left: Option[Node[T]]): Node[T] = {
    this.left = left
    this
  }

  def withRight(right: Option[Node[T]]): Node[T] = {
    this.right = right
    this
  }

  def asBlack: Node[T]

  def asRed: Node[T]
}

case class Red[T](override val value: T) extends Node[T](value) {

  override def asBlack: Node[T] = Black(value).withLeft(this.left).withRight(this.right)

  override def asRed: Node[T] = this

}

case class Black[T](override val value: T) extends Node[T](value) {

  override def asBlack: Node[T] = this

  override def asRed: Node[T] = Red(value).withLeft(this.left).withRight(this.right)

}

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
    *
    * @param value value to insert
    */
  def insert(value: T): Unit = root match {
    case None => root = Some(Black(value)); currentSize += 1
    case Some(_) => nonRootInsert(value, root, None, List.empty); currentSize += 1
  }

  /**
    * We try to make the newly inserted element Red. If it's parent is black then WHOOP we are done, because all the invariants
    * remain unbroken.
    *
    * However if the parent of the newly insert red node is also red then we have a little problem because we are not meant to have
    * double red nodes in our tree.
    *
    * Our resurrectInvariants method handles this... (so go look there for the next step)
    *
    * @param value   new value to insert into tree
    * @param current node we are looking at (if this node isnt defined then we can insert here)
    * @param parent  parent of current node
    */
  private def nonRootInsert(value: T, current: Option[Node[T]], parent: Option[Node[T]], routeToNode: List[Node[T]]): Unit = (current, parent) match {
    case (None, Some(p)) if ord.gt(p.value, value) => {
      p.left = Some(Red(value))
      resurrectInvariants(p.left.get +: routeToNode)
    };
    case (None, Some(p)) if ord.lt(p.value, value) => {
      p.right = Some(Red(value))
      resurrectInvariants(p.right.get +: routeToNode)
    };
    case (Some(node), _) if ord.gt(node.value, value) => nonRootInsert(value, node.left, current, node +: routeToNode)
    case (Some(node), _) if ord.lt(node.value, value) => nonRootInsert(value, node.right, current, node +: routeToNode)
  }

  /**
    * @param routeToNewlyInsertedNode
    */
  private def resurrectInvariants(routeToNewlyInsertedNode: List[Node[T]]): Unit = routeToNewlyInsertedNode match {
    case Red(_) :: Black(_) :: ns => // everything is lovely and we can stop
    case Red(_) :: Red(_) :: n3 :: ns => {
      // the parent of n2 (n3) must be black and any other child of n2 must be Red
      //                 n3 (B)
      //                /      \
      //      some_child (R)   n2(R)
      //                       /
      //                      n1(R)
      //
      // Therefore we must swap things around so our tree invariants hold again
      // start by recolouring some_child and n2 to black and make n2_parent red
      //

      n3.left.foreach(n => n3.left = Some(n.asBlack))
      n3.right.foreach(n => n3.right = Some(n.asBlack))

      // if n3 has a parent
      if (ns.nonEmpty) {
        val n4 = ns.head

        // set n3 = red (we do the n3 parent check because we dont set it to red if it is the root because INVARIANTS!)
        n4.left.filter(_.value == n3.value).foreach(l => n4.left = Some(l.asRed))
        n4.right.filter(_.value == n3.value).foreach(r => n4.right = Some(r.asRed))

        n4 match {
          case Red(_) => {
            // bad! we have Red(n4) -> Red(n3) which is a violation! Must recurse!
            resurrectInvariants(n3 :: ns)
          }
          case Black(_) => // no more violations because we have Black(n4) -> Red(n3) -> (Black(n1), Black(n2))
        }
      }
    }
  }

  def inOrder: Seq[T] = inOrderTraversalFrom(root)

  private def inOrderTraversalFrom(node: Option[Node[T]], result: List[T] = List.empty): List[T] = node match {
    case Some(n) => (inOrderTraversalFrom(n.left) :+ n.value) ++ inOrderTraversalFrom(n.right)
    case None => result
  }
}
