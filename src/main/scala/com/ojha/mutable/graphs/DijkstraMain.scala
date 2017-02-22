package com.ojha.mutable.graphs

object DijkstraMain extends App {

  import scala.io.Source

  val graph = DirectedGraph[Int]()
  val max =  200
  val nodes =  (1 to max).map(Node(_))
  nodes.foreach(graph.addNode)

  val reader = Source.fromResource("dijkstra/dijkstraGraph.txt").getLines.buffered
  reader.foreach(line => {
    val lineData = line.split("\t").toList
    val node = Node(lineData.head.toInt)
    lineData.tail.map(_.split(",").map(_.toInt)).foreach(edgeInfo => graph.addWeightedEdge(node, Node(edgeInfo(0)), edgeInfo(1)))
  })

  val paths = graph.shortestWeightedPathsToAllOtherNodes(Node(1))

  //2599,2610,2947,2052,2367,2399,2029,2442,2505,3068

  println(Seq(7,37,59,82,99,115,133,165,188,197).map(i => paths(Node(i))._2).mkString(","))


}
