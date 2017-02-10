//package com.ojha.graph
//
//case class ExplorableNode[T <: Ordered[T]](override val value: T) extends Node[T] {
//
//  var explored = false
//
//  override var connections: Seq[ExplorableNode[T]] = Seq()
//
//  def explore() = explored = true
//  def unexplore() = explored = false
//
//}
//
//
