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

  /**
    * A topological sort for a directed acyclic graph is a ordered list of nodes
    * such that for every edge, u -> v, in the graph u comes before v in the ordering
    * @return nodes in topological sort order
    */
  def topologicalSort: List[Node[T]] = {
    adjacencyList.keySet
      .foldLeft(List.empty[Node[T]])((acc, next) => dfs(next).filterNot(acc.contains) ++ acc)
  }

  /**
    * @param sort
    * @return true if the list is a valid topological sort for the graph
    */
  def isTopologicalSort(sort: List[Node[T]]): Boolean = {
    val indexedSort = sort.zipWithIndex.toMap
    sort.forall(node => indexedSort.contains(node) &&
      adjacencyList(node).forall(edge => indexedSort.contains(edge.to) &&
                                         indexedSort(edge.to) > indexedSort(node)))
  }

}
