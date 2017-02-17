package com.ojha.mutable.graphs

object DirectedGraph {

  def apply[T](): DirectedGraph[T] = new DirectedGraph[T]

  def apply[T](nodes: Node[T]*): DirectedGraph[T] = {
    val graph = new DirectedGraph[T]
    graph.addNodes(nodes)
    graph
  }

}

class DirectedGraph[T] extends Graph[T] {

  /**
    * Add one unidirectional edge
    *
    * @param fromToPair
    */
  def addEdge(fromToPair: (Node[T], Node[T]) ) = {
    val edge = UnidirectionalEdge(fromToPair._1, fromToPair._2)
    edge +: edges
    adjacencyList(fromToPair._1) = edge +: adjacencyList(fromToPair._1)
  }

  def topologicalSort: List[Node[T]] = {
    var visited = List.empty[Node[T]]
    adjacencyList.keySet.foreach(k => {
      if (!visited.contains(k)) {
        visited = dfs(k).filterNot(visited.contains) ++ visited
        println(visited.mkString(" "))
      }
    })
    visited
  }

  def isTopologicalSort(sort: List[Node[T]]): Boolean = {
    val indexedSort = sort.zipWithIndex.toMap
    sort.forall(node => indexedSort.contains(node) &&
      adjacencyList(node).forall(edge => indexedSort.contains(edge.to) &&
                                         indexedSort(edge.to) > indexedSort(node)))
  }

}
