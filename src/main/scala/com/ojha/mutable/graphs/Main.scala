package com.ojha.mutable.graphs

/**
  * Created by alexbate on 2/19/17.
  */
object Main extends App {

  import scala.io.Source

  val graph = DirectedGraph[Int]()
  val nodes =  (1 to 875714).map(Node(_))
  nodes.foreach(graph.addNode)
  println("Got nodez")

  val reader = Source.fromResource("Cat.txt").getLines.buffered
  reader.foreach(line => {
    val pair = line.split(" ").map(_.toInt)
    graph.addEdge((nodes(pair(0) - 1), nodes(pair(1) - 1)))
  })

  println("Got edges")

  println(graph.stronglyConnected.map(_.size).sorted)


}
