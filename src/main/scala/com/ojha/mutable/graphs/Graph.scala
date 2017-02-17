package com.ojha.mutable.graphs

import scala.collection.mutable

trait Graph[T] {

  /**
    * Store a map of Node to its outgoing edges
    */
  protected val adjacencyList = mutable.Map.empty[Node[T], Seq[UnidirectionalEdge[T]]]

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
  def addNode(node: Node[T]) = adjacencyList(node) = mutable.Seq.empty[UnidirectionalEdge[T]]


  /**
    * @param edgeDefinitions
    */
  def addEdges(edgeDefinitions: (Node[T], Node[T])*) = edgeDefinitions foreach addEdge

  def addEdge(pair: (Node[T], Node[T])): Unit

  /**
    * @param node
    * @return list of nodes to which there exists a path of unit length from the input node
    */
  def getOutgoingNodes(node: Node[T]): Seq[Node[T]] = adjacencyList(node).map(_.to)

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
        adjacencyList(node).map(_.to).filter(!visited.contains(_)).foreach(q.enqueue(_))
        aux(visited + node)
      }
    }

    aux()
  }

  /**
    * @param start
    * @return depth first search from start
    */
  def dfs(start: Node[T]): Set[Node[T]] = {
    val stack = mutable.ArrayBuffer(start)

    def aux(visited: Set[Node[T]] = Set.empty): Set[Node[T]] = {
      if (stack.isEmpty) visited
      else {
        val node = stack.remove(0)
        adjacencyList(node).map(_.to).filter(!visited.contains(_)).foreach(stack.insert(stack.length, _))
        aux(visited + node)
      }
    }
    aux()
  }

//  def topologicalSort
}


case class UnidirectionalEdge[T](from: Node[T], to: Node[T])
