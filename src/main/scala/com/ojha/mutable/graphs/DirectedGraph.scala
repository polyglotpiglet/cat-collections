package com.ojha.mutable.graphs

import scala.collection.mutable

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
    * Store a map of Node to its incoming edges
    */
  protected val adjacencyListForIncomingNodes = mutable.Map.empty[Node[T], Seq[UnidirectionalEdge[T]]]

  /**
    * Add single node to graph
    *
    * @param node
    */
  override def addNode(node: Node[T]) = {
    adjacencyListForOutgoingNodes(node) = mutable.Seq.empty[UnidirectionalEdge[T]]
    adjacencyListForIncomingNodes(node) = mutable.Seq.empty[UnidirectionalEdge[T]]
  }

  /**
    * @param node
    * @return list of nodes to which there exists a path of unit length from the input node
    */
  def getIncomingNodes(node: Node[T]): Seq[Node[T]] = adjacencyListForIncomingNodes(node).map(_.to)

  /**
    * Add one unidirectional edge
    *
    * @param fromToPair
    */
  def addEdge(fromToPair: (Node[T], Node[T]) ) = {
    val edge = UnidirectionalEdge(fromToPair._1, fromToPair._2)
    edge +: edges
    adjacencyListForOutgoingNodes(fromToPair._1) = edge +: adjacencyListForOutgoingNodes(fromToPair._1)
    adjacencyListForIncomingNodes(fromToPair._2) = UnidirectionalEdge(edge.to, edge.from) +: adjacencyListForIncomingNodes(fromToPair._2)
  }

  /**
    * A topological sort for a directed acyclic graph is a ordered list of nodes
    * such that for every edge, u -> v, in the graph u comes before v in the ordering
    *
    * @return nodes in topological sort order
    */
  def topologicalSort: Seq[Node[T]] = {
    adjacencyListForOutgoingNodes.keySet
      .foldLeft(Seq.empty[Node[T]])((acc, next) => preOrderDfs(next).filterNot(acc.contains) ++ acc)
  }

  /**
    * @param sort
    * @return true if the list is a valid topological sort for the graph
    */
  def isTopologicalSort(sort: Seq[Node[T]]): Boolean = {
    val indexedSort = sort.zipWithIndex.toMap
    sort.forall(node => indexedSort.contains(node) &&
      adjacencyListForOutgoingNodes(node).forall(edge => indexedSort.contains(edge.to) &&
                                         indexedSort(edge.to) > indexedSort(node)))
  }

  def reverse() = {
    val temp = this.adjacencyListForIncomingNodes
    val nodes =  adjacencyListForIncomingNodes.keys
    nodes.foreach(node => adjacencyListForIncomingNodes(node) = adjacencyListForOutgoingNodes(node))
    nodes.foreach(node => adjacencyListForOutgoingNodes(node) = temp(node))
  }

  def stronglyConnected: Seq[Seq[Node[T]]] = {

    val start = (Map.empty[Node[T], Int], 0)
    val order = adjacencyListForIncomingNodes.keys.foldLeft(start) ((acc, next) => {
      val (visited, index) = acc
      if (!visited.contains(next)) {
        val toAdd = preOrderDfsForAdjacencyList(adjacencyListForIncomingNodes, next, visited.keySet)//.filterNot(visited.contains)
//        println(s"${next.value} | ${toAdd.mkString(" ")}")
        val size = toAdd.size
        val zipped = toAdd.zip(index until index + size)
        (visited ++ zipped, index + size)
      }
      else acc
    })._1.toList.sortBy(_._2).map(_._1)
//    println("got order " + order.mkString(" "))
    println("got order")

    order.foldLeft(Set.empty[Node[T]], Seq.empty[Seq[Node[T]]]) ((acc, next) => {
      val (visited, partialResult) = acc
      if (!visited.contains(next)) {

        val toAdd = preOrderDfsForAdjacencyList(adjacencyListForOutgoingNodes, next, visited)//.filterNot(visited.contains)
//        println("DFSing for " + next.value + " | " + toAdd.mkString(" "))
        (visited ++ toAdd, partialResult.+:(toAdd))
      }
      else acc
    })._2
  }
}
