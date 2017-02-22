package com.ojha.mutable.graphs

object StronglyConnectedComponentsMain extends App {

  import scala.io.Source

  val graph = DirectedGraph[Int]()
  val max =  875714
  val nodes =  (1 to max).map(Node(_))
  nodes.foreach(graph.addNode)

  val reader = Source.fromResource("VeryBigExampleGraph.txt").getLines.buffered
  reader.foreach(line => {
    val pair = line.split(" ").map(_.toInt)
    graph.addUnitEdge((nodes(pair(0) - 1), nodes(pair(1) - 1)))
  })

  println("Got edges")
  val result = graph.stronglyConnected

  println(result.map(_.size).sorted.reverse.take(5).mkString(","))


}
