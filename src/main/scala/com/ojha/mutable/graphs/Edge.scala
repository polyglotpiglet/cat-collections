package com.ojha.mutable.graphs

sealed abstract class Edge[T]

case class DirectedEdge[T](from: Node[T], to: Node[T]) extends Edge[T]

case class UndirectedEdge[T](node1: Node[T], node2: Node[T]) extends Edge[T]



