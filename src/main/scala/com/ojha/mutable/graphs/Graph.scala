package com.ojha.mutable.graphs

import sun.security.provider.certpath.AdjacencyList

import scala.annotation.tailrec
import scala.collection.mutable

trait Graph[T] {

  type AdjacencyList = mutable.Map[Node[T], Seq[UnidirectionalEdge[T]]]

  /**
    * Store a map of Node to its outgoing edges
    */
  protected val adjacencyListForOutgoingNodes: AdjacencyList = mutable.Map.empty

  /**
    * Also maintian a list of edges
    */
  protected val edges = mutable.Seq.empty[UnidirectionalEdge[T]]

  /**
    * Bulk add nodes to the graph
    *
    * @param nodes
    */
  def addNodes(nodes: Seq[Node[T]]) = nodes foreach addNode

  /**
    * Add single node to graph
    *
    * @param node
    */
  def addNode(node: Node[T]) = adjacencyListForOutgoingNodes(node) = mutable.Seq.empty[UnidirectionalEdge[T]]

  /**
    * @param edgeDefinitions
    */
  def addEdges(edgeDefinitions: (Node[T], Node[T])*) = edgeDefinitions foreach addEdge

  def addEdge(pair: (Node[T], Node[T])): Unit

  /**
    * @param node
    * @return list of nodes to which there exists a path of unit length from the input node
    */
  def getOutgoingNodes(node: Node[T]): Seq[Node[T]] = adjacencyListForOutgoingNodes(node).map(_.to)

  /**
    * @param start
    * @return breadth first search from start
    */
  def bfs(start: Node[T]): Set[Node[T]] = {
    val q = new mutable.Queue[Node[T]]()
    q.enqueue(start)

    def aux(visited: Set[Node[T]] = Set.empty): Set[Node[T]] = {
      if (q.isEmpty) visited
      else {
        val node = q.dequeue()
        adjacencyListForOutgoingNodes(node).map(_.to).filter(!visited.contains(_)).foreach(q.enqueue(_))
        aux(visited + node)
      }
    }
    aux()
  }

  def shortestPath(from: Node[T], to: Node[T]) = {

    val q = new mutable.Queue[(Node[T], Seq[Node[T]])]()
    q.enqueue((from, Seq.empty))

    def aux(visited: Seq[Node[T]] = Seq.empty): Seq[Node[T]] = {
      if (q.isEmpty) Seq.empty
      else {
        val (node, pathToNode) = q.dequeue()
        if (node == to) pathToNode.:+(node)
        else {
          adjacencyListForOutgoingNodes(node)
            .map(_.to)
            .filter(!visited.contains(_))
            .foreach(child => q.enqueue((child, pathToNode.:+(node))))
          aux(visited.:+(node))
        }
      }
    }
    aux()
  }

  /**
    * @param start
    * @return depth first search from start
    */
  def preOrderDfs(start: Node[T]): Seq[Node[T]] = {
    preOrderDfsForAdjacencyList(adjacencyListForOutgoingNodes, start)
  }

  def postOrderDfs(start: Node[T]): Seq[Node[T]] = {
    postOrderDfsForAdjacencyList(adjacencyListForOutgoingNodes, start)
  }

  @tailrec
  final protected def preOrderDfsForAdjacencyList(adjacencyList: AdjacencyList, stack: List[Node[T]], visited: Set[Node[T]], partialResult: Seq[Node[T]]): Seq[Node[T]] = {
    stack match {
      case Nil => partialResult.reverse
      case node :: rest => {
        val unexploredNodes = adjacencyList(node).map(_.to).filterNot(visited.contains).toList.reverse
        preOrderDfsForAdjacencyList(adjacencyList, unexploredNodes.foldLeft(rest)(_.+:(_)), unexploredNodes.foldLeft(visited)(_ + _), partialResult.+:(node))
      }
    }
  }

  final protected def preOrderDfsForAdjacencyList(adjacencyList: AdjacencyList, start: Node[T], visited: Set[Node[T]] = Set.empty): Seq[Node[T]] = {
    this.preOrderDfsForAdjacencyList(adjacencyList, List(start), visited.+(start), Seq.empty)
  }

  /**
    *
    * @param adjacencyList
    * @param stack
    * @param visited set of nodes that have been visited or are in the stack waiting to be visisted
    * @param partialResult
    * @return
    */
  @tailrec
  final private def postOrderDfsForAdjacencyList(adjacencyList: AdjacencyList, stack: List[Node[T]], visited: Set[Node[T]], partialResult: Seq[Node[T]]): Seq[Node[T]] = {
    stack match {
      case Nil => partialResult.reverse
      case node :: rest  => {
        val unExploredConnections = adjacencyList(node).map(_.to).filterNot(visited.contains).toList
        if (unExploredConnections.nonEmpty) {
          postOrderDfsForAdjacencyList(adjacencyList, unExploredConnections ++ stack, unExploredConnections.foldLeft(visited)((acc, next)=>acc.+(next)), partialResult)
        }
        else {
          postOrderDfsForAdjacencyList(adjacencyList, rest, visited.+(node), node +: partialResult)
        }
      }
    }
  }

  final protected def postOrderDfsForAdjacencyList(adjacencyList: AdjacencyList, start: Node[T], visited: Set[Node[T]] = Set.empty): Seq[Node[T]] = {
    this.postOrderDfsForAdjacencyList(adjacencyList, List(start), visited.+(start), Seq.empty)
  }

}


case class UnidirectionalEdge[T](from: Node[T], to: Node[T])
