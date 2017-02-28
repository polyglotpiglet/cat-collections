package com.ojha.mutable.trees

import com.ojha.mutable.ArrayBackedTree

/**
  *
  * A red black tree is a beautiful thing.
  *
  * The laws of red black trees are the following:
  *
  * - Every node is red or black
  * - The root must be black
  * - All leaves are black
  * - Red nodes mustn't have red children (which also means no red can have a red parent)
  * - The paths from the root to any Nil node must all contain the same number of black nodes
  *
  * Eg
  *
  *     Black
  *  Red    Red
  *
  *  ^ valid red black tree ^
  *
  *  Eg
  *
  *      Red
  *  Black Black
  *
  *  ^ invalid red black tree because the root isn't black ^
  *
  *  Eg
  *
  *     Black
  *   Red   Red
  *  Red
  *
  *  ^ invalid red black tree because there is a red - red parent child relationship ^
  *
  *  Eg
  *
  *          Black
  *       Red    Red
  *  Black
  *
  *  ^ invalid because Root -> Left -> Left -> Null contains two blacks but other routes to null contain only one ^
  *
  */

object RedBlackTree {
  def empty[T](implicit ord: Ordering[T]) = new RedBlackTree[T]()
}

/**
  * @param ord
  * @tparam T
  */
class RedBlackTree[T](implicit ord: Ordering[T]) extends ArrayBackedTree[T] {

  override def insert(value: T): Unit = ???

}
