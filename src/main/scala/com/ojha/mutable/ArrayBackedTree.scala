package com.ojha.mutable

import com.ojha.mutable.trees.Tree

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer

abstract class ArrayBackedTree[T] extends Tree[T] {

  protected val backing: ArrayBuffer[T] = mutable.ArrayBuffer.empty[T]

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
  def parentIndex(childIndex: Int): Int = (childIndex - 1) / 2


  @Override
  def size: Int = backing.size

  @Override
  def isEmpty: Boolean = backing.isEmpty

  @Override
  def nonEmpty: Boolean = backing.nonEmpty


}
