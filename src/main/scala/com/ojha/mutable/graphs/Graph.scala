package com.ojha.mutable.graphs

import scala.collection.mutable

object Graph {

  def apply[T](): Graph[T] = new Graph[T]

  def apply[T](nodes: Node[T]*): Graph[T] = {
    val graph = new Graph[T]
    graph.addNodes(nodes)
    graph
  }
}
class Graph[T] {


  /**
    * Store a map of Node to its outgoing edges
    */
  private val adjacencyList = mutable.Map.empty[Node[T], Seq[UnidirectionalEdge[T]]]

  /**
    * Also maintian a list of edges
    */
  private val edges = mutable.Seq.empty[UnidirectionalEdge[T]]

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
    * Bulk add unidirectional edges
    *
    * @param edgeDefinitions
    */
  def addDirectedEdges(edgeDefinitions: (Node[T], Node[T])*) = edgeDefinitions foreach addDirectedEdge

  /**
    * Add one unidirectional edge
    *
    * @param fromToPair
    */
  def addDirectedEdge(fromToPair: (Node[T], Node[T]) ) = {
    val edge = UnidirectionalEdge(fromToPair._1, fromToPair._2)
    edge +: edges
    adjacencyList(fromToPair._1) = edge +: adjacencyList(fromToPair._1)
  }

  /**
    * Bulk add unidirectional edges
    *
    * @param edgeDefinitions
    */
  def addUndirectedEdges(edgeDefinitions: (Node[T], Node[T])*) = edgeDefinitions foreach addUndirectedEdge

  def addUndirectedEdge(pair: (Node[T], Node[T]) ) = {
    val first = UnidirectionalEdge(pair._1, pair._2)
    val second = UnidirectionalEdge(pair._2, pair._1)
    first +: edges
    second +: edges
    adjacencyList(pair._1) = first +: adjacencyList(pair._1)
    adjacencyList(pair._2) = second +: adjacencyList(pair._2)
  }

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
}


case class UnidirectionalEdge[T](from: Node[T], to: Node[T])
