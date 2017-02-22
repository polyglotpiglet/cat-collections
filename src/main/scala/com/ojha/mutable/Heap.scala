package com.ojha.mutable

import scala.collection.mutable
import scala.math.Ordering

object Heap {
  def empty[T](implicit ord: Ordering[T]) = new Heap[T]
}

class Heap[T] (implicit val ord: Ordering[T]){

  private val backing = mutable.ArrayBuffer.empty[T]

  /**
    * Insert an item into the heap
    * Inserts to any leaf, then bubbles up such that heap remains sorted
    */
  def insert(value: T) = {
    val insertIndex = backing.length
    backing.insert(insertIndex, value)
    bubbleUpFrom(insertIndex)
  }

  /**
    * @return max value in heap
    */
  def top = backing(0)

  /**
    * Remove and return the root of the heap (ie the max value)
    *
    * @param ord
    * @return top of array
    */
  def popTop()(implicit ord: Ordering[T]): T = {
    if (backing.size == 1) backing.remove(0)
    else {
      val topBeforeRemoval = top
      val leaf = backing.remove(backing.length - 1)
      backing(0) = leaf
      bubbleDownFrom(0)
      topBeforeRemoval
    }
  }

  /** *
    *
    * @param parentIndex index in backing array of parent
    * @return returns hypothetical left child index (hypothetical because child might be empty)
    *
    *         0 1  2   3   4   5   6   7   8    9    10   11   12   13   14   15   <- Index
    *         1  3  5  7   9   11 ...                                              <- Left child index
    *         N N1 N2 N11 N12 N21 N22 N111 N112 N121 N122 N211 N212 N221 N222      <- Nodes (N11 children are N111 and N112)
    *
    */

  def childIndex(parentIndex: Int): Int = parentIndex * 2 + 1

  /**
    *
    * @param childIndex
    * @return index of parent in backing array
    */
  def parentIndex(childIndex: Int) = (childIndex - 1) / 2


  /**
    *
    * @param insertIndex start from this lower level and keep swapping with parent until heap property satisfied
    * @param ord
    */
  private def bubbleUpFrom(insertIndex: Int)(implicit ord: Ordering[T]): Unit = {
    val parent = parentIndex(insertIndex)

    if (parent >= 0 && ord.lt(backing(parent), backing(insertIndex))) {
      swap(parent, insertIndex)
      bubbleUpFrom(parent)
    }
  }

  /**
    *
    * @param index bubble down from this index
    * @param ord
    *
    *              Swap item at this index with the max of its children until heap property is satisfied again
    */
  private def bubbleDownFrom(index: Int)(implicit ord: Ordering[T]): Unit = {
    val child = childIndex(index)
    if (child < backing.length) {

      val toSwap = (child, child + 1) match {
        case (i, j) if j == backing.length => i
        case (i, j) if ord.gt(backing(i), backing(j)) => i
        case (_, j) => j
      }
      if (ord.gt(backing(toSwap), backing(index))) {
        swap(toSwap, index)
        bubbleDownFrom(toSwap)
      }
    }
  }

  /** *
    *
    * @param i
    * @param j
    *
    * Swap elements i and j in backing array
    */
  private def swap(i: Int, j: Int): Unit = {
    val t = backing(i)
    backing(i) = backing(j)
    backing(j) = t
  }

  /**
    * @param value value to be found
    * @param index check this index and its children
    * @param ord
    * @return optional index of value in heap's backing array
    */
  private def find(value: T, index: Int = 0)(implicit ord: Ordering[T]): Option[Int] = {
    val leftChildIndex = childIndex(index)
    val rightChildIndex = leftChildIndex + 1
    val heapSize = backing.length

    if (ord.equiv(value, backing(index))) Some(index)
    else if (leftChildIndex < heapSize && ord.gteq(backing(leftChildIndex), value)) {
        find(value, leftChildIndex).orElse(
          if (rightChildIndex < heapSize && ord.gteq(backing(rightChildIndex), value)) find(value ,rightChildIndex)
          else None
        )
      }
    else None
  }

  /**
    * Finds index of value in heap, bubbles to top using synthetic ordering then popsTop
    *
    * @param value toDelete
    * @param ord   natural ordering of elements in heap
    * @return
    */
  def deleteValue(value: T)(implicit ord: Ordering[T]) = {
    val index = find(value)

    val orderingToSendValueToTop = new Ordering[T] {
      override def compare(x: T, y: T): Int = {
        if (ord.equiv(x, value)) 1
        else if (ord.equiv(y, value)) -1
        else ord.compare(x, y)
      }
    }

    index match {
      case Some(i) => {
        bubbleUpFrom(i)(orderingToSendValueToTop)
        popTop()
      }
      case None =>
    }
  }

}
