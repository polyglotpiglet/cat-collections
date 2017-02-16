package com.ojha.mutable

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
    * @param nodes
    */
  def addNodes(nodes: Seq[Node[T]]) = nodes foreach addNode

  /**
    * Add single node to graph
    * @param node
    */
  def addNode(node: Node[T]) = adjacencyList(node) = mutable.Seq.empty[UnidirectionalEdge[T]]


  def addEdges(edgeDefinitions: (Node[T], Node[T])*) = edgeDefinitions foreach addEdge

  def addEdge(fromToPair: (Node[T], Node[T]) ) = {
    val edge = UnidirectionalEdge(fromToPair._1, fromToPair._2)
    edge +: edges
    adjacencyList(fromToPair._1) = edge +: adjacencyList(fromToPair._1)
  }


  /**
    * @param node
    * @return list of nodes to which there exists a path of unit length from the input node
    */
  def getOutgoingNodes(node: Node[T]): Seq[Node[T]] = adjacencyList(node).map(_.to)

}

case class Node[T](value: T)

case class UnidirectionalEdge[T](from: Node[T], to: Node[T])
