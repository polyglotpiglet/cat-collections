package com.ojha.mutable

import org.scalatest.{FlatSpec, Matchers}

class GraphSuite extends FlatSpec with Matchers {

  it should "correctly return outgoing connections" in {
    val s = Node("s")
    val a = Node("a")
    val b = Node("b")
    val c = Node("c")
    val d = Node("d")
    val e = Node("e")

    val graph = Graph(s,a,b,c,d,e)
    graph addEdges((s, a),
                   (s, b),
                   (a, c),
                   (b, c),
                   (b, d),
                   (c, d),
                   (c, e),
                   (d, e))


    graph.getOutgoingNodes(s) should contain theSameElementsAs Seq(a,b)
    graph.getOutgoingNodes(a) should contain theSameElementsAs Seq(c)
    graph.getOutgoingNodes(b) should contain theSameElementsAs Seq(c,d)
    graph.getOutgoingNodes(c) should contain theSameElementsAs Seq(e,d)
    graph.getOutgoingNodes(d) should contain theSameElementsAs Seq(e)
    graph.getOutgoingNodes(e) should contain theSameElementsAs Seq()

  }

}
