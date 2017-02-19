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


  /**
    * Add a bidirectional edge between pair._1 and pair._2
   */
  def addEdge(pair: (Node[T], Node[T]) ) = {
    val first = UnidirectionalEdge(pair._1, pair._2)
    val second = UnidirectionalEdge(pair._2, pair._1)
    first +: edges
    second +: edges
    adjacencyListForOutgoingNodes(pair._1) = first +: adjacencyListForOutgoingNodes(pair._1)
    adjacencyListForOutgoingNodes(pair._2) = second +: adjacencyListForOutgoingNodes(pair._2)
  }

  override def preOrderDfs(start: Node[T]): Seq[Node[T]] = super.preOrderDfs(start).reverse

  /**
    * The diameter of a graph is the maximum, over all choices of vertices s and t,
    * of the shortest path distances between s and t.
    * Node that the shortest path between is the number of nodes in the path not the number of
    * edges.
    * Ie if path was s -> a -> t then the path size would be 3. An alternative definition would
    * be 2 (= number of edges between s and t).
    *
    * @return diameter of the graph
    */
  def diameter(): Int = {
    val nodes = adjacencyListForOutgoingNodes.keys.toArray
    val allPaths = nodes.indices.flatMap(i => (i until nodes.length).map(j => shortestPath(nodes(i), nodes(j))))
    allPaths.map(_.length).max
  }

  /* TODO optimise because this is shit slow rofl */
  /**
    * For each node s in a graph, f(s) is the maximum distance to another node in the graph.
    * The radius of a graph is the minimum f(s) over all choices of s (ie over all nodes in the graph
    * @return radius of the graph
    */
  def radius: Int = {
    val nodes = adjacencyListForOutgoingNodes.keys.toArray
    nodes.map(node => nodes.map(other => shortestPath(node, other)).map(_.length).max).min
  }

}
