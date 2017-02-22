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
    * Uses dijkstra to compute shortest path for graph with weighted edges
    *
    * @param from
    * @param to
    * @return tuple: (route between from and to, total weight of route)
    */
  def shortestWeightedPath(from: Node[T], to: Node[T]): (Seq[Node[T]], Int) = {
    shortestWeightedPathsToAllOtherNodes(from)(to)
  }

  var count = 0
  def shortestWeightedPathsToAllOtherNodes(from: Node[T]): Map[Node[T], (Seq[Node[T]], Int)] = {





    val nodes = mutable.HashSet().++(adjacencyListForIncomingNodes.keys)
    val tentativeDistances = adjacencyListForIncomingNodes.keySet.map(node => (node, (Seq.empty[Node[T]], Int.MaxValue))).toMap.updated(from, (Seq(from), 0))

    aux(from, tentativeDistances, nodes)
  }

  def aux(currentNode: Node[T],
          tentativeDistances: Map[Node[T], (Seq[Node[T]], Int)],
          unvisitedNodes: mutable.HashSet[Node[T]]): Map[Node[T], (Seq[Node[T]], Int)] = {
    count += 1

    if (unvisitedNodes.isEmpty) return tentativeDistances

    val (pathToCurrentNode, weightToCurrentNode) = tentativeDistances(currentNode)

    val tentativeDistancesToUnvisitedNeighbours = this.adjacencyListForOutgoingNodes(currentNode)
      .filter(edge => unvisitedNodes.contains(edge.to))
      .map(edge => (edge.to, edge.weight + weightToCurrentNode))
      .toMap

    val updatedTentativeDistances = tentativeDistances.map { case (node, (route, weight)) =>
      if (tentativeDistancesToUnvisitedNeighbours.contains(node) && tentativeDistancesToUnvisitedNeighbours(node) < weight) {
        (node, (pathToCurrentNode.:+(node), tentativeDistancesToUnvisitedNeighbours(node)))
      }
      else (node, (route, weight))
    }

    val visitNode = unvisitedNodes.-(currentNode)
    if (visitNode.nonEmpty) {
      val nextNode = visitNode.minBy(node => updatedTentativeDistances(node)._2)
      aux(nextNode, updatedTentativeDistances, visitNode)
    }
    else {
      updatedTentativeDistances
    }
  }

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
  def addUnitEdge(fromToPair: (Node[T], Node[T])) = {
    val edge = UnidirectionalEdge(fromToPair._1, fromToPair._2)
    edge +: edges
    adjacencyListForOutgoingNodes(fromToPair._1) = edge +: adjacencyListForOutgoingNodes(fromToPair._1)
    adjacencyListForIncomingNodes(fromToPair._2) = UnidirectionalEdge(edge.to, edge.from) +: adjacencyListForIncomingNodes(fromToPair._2)
  }


  def addWeightedEdge(fromToPairWithWeight: (Node[T], Node[T], Int)) = {
    val edge = UnidirectionalEdge(fromToPairWithWeight._1, fromToPairWithWeight._2, fromToPairWithWeight._3)
    edge +: edges
    adjacencyListForOutgoingNodes(fromToPairWithWeight._1) = edge +: adjacencyListForOutgoingNodes(fromToPairWithWeight._1)
    adjacencyListForIncomingNodes(fromToPairWithWeight._2) = UnidirectionalEdge(edge.to, edge.from) +: adjacencyListForIncomingNodes(fromToPairWithWeight._2)
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
    val nodes = adjacencyListForIncomingNodes.keys
    nodes.foreach(node => adjacencyListForIncomingNodes(node) = adjacencyListForOutgoingNodes(node))
    nodes.foreach(node => adjacencyListForOutgoingNodes(node) = temp(node))
  }

  /**
    * A strongly connected component in a graph is a collection of nodes
    * where every node in the collection is reachable by every other node
    *
    * @return All the strongly connected components in the graph
    */
  def stronglyConnected: Seq[Seq[Node[T]]] = {
    val start = (Seq.empty[Node[T]], Set.empty[Node[T]])
    val order = adjacencyListForIncomingNodes.keys.foldLeft(start) ((acc, next) => {
      val (result, visited) = acc
      if (!visited.contains(next)) {
        val toAdd = preOrderDfsForAdjacencyList(adjacencyListForIncomingNodes, next, visited)

        // quicker to fold left over result than try and do super slow concat for large graphs
        (toAdd.reverse.foldLeft(result)(_.+:(_)), toAdd.foldLeft(visited)(_ + _))
      }
      else acc
    })._1

    order.foldLeft(Set.empty[Node[T]], Seq.empty[Seq[Node[T]]]) ((acc, next) => {
      val (visited, partialResult) = acc
      if (!visited.contains(next)) {
        val toAdd = preOrderDfsForAdjacencyList(adjacencyListForOutgoingNodes, next, visited)
        (visited ++ toAdd, partialResult.+:(toAdd))
      }
      else acc
    })._2
  }
}
