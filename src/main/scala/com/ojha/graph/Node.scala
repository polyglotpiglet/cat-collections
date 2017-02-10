package com.ojha.graph

trait Node[T <: Ordered[T]] extends Ordered[Node[T]] {

  val value: T
  var connections: Seq[Node[T]] = Seq()

  def addConnections(n: Node[T]*) { connections = n ++ connections}

  def compare(that: Node[T]) = this.value.compareTo(that.value)

}
