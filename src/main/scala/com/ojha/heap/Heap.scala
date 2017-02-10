//package com.ojha.heap
//
//object Heap {
//
//  def empty[T<: Ordered]: Heap[T] = new Empty[T]
//
//}
//
//abstract class Heap[T <: Ordered[T]] {
//  def updated(that: T): Heap[T]
//  def isEmpty: Boolean
//  def size: Int
//
//}
//
//case class Empty[T]() extends Heap[T] {
//  override def isEmpty: Boolean = true
//
//  override def size: Int = 0
//
//  override def updated(that: T): Heap[T] = new Leaf[T](that)
//}
//
//case class Leaf[T <: Ordered[T]](value: T) extends Heap[T] {
//
//  override def isEmpty: Boolean = false
//
//  override def size: Int = 1
//
//  override def updated(that: T): Heap[T] = new Fork(this,new Leaf(that))
//}
//
//case class Fork[T <: Ordered[T]](left: Heap[T], right: Heap[T]) extends Heap[T] {
//  override def isEmpty: Boolean = false
//
//  override lazy val size: Int = right.size + left.size
//
//  override def updated(that: T): Heap[T] = {
//    (left, right) match {
//      case (Leaf(v1), _) => {
//        val updatedLeft = left.updated(that)
//        null
//
//      }
//
//    }
//  }
//}
