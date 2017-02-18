package com.ojha.mutable.graphs

object UndirectedGraph {

  def apply[T](): UndirectedGraph[T] = new UndirectedGraph[T]

  def apply[T](nodes: Node[T]*): UndirectedGraph[T] = {
    val graph = new UndirectedGraph[T]
    graph.addNodes(nodes)
    graph
  }

}

class UndirectedGraph[T] extends Graph[T] {

  def addEdge(pair: (Node[T], Node[T]) ) = {
    val first = UnidirectionalEdge(pair._1, pair._2)
    val second = UnidirectionalEdge(pair._2, pair._1)
    first +: edges
    second +: edges
    adjacencyListForOutgoingNodes(pair._1) = first +: adjacencyListForOutgoingNodes(pair._1)
    adjacencyListForOutgoingNodes(pair._2) = second +: adjacencyListForOutgoingNodes(pair._2)
  }

}
