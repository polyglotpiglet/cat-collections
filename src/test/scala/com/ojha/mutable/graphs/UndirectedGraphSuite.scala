package com.ojha.mutable.graphs

import org.scalatest.{FlatSpec, Matchers}

class UndirectedGraphSuite extends FlatSpec with Matchers {

  it should "correctly return outgoing connections for undirected edges" in {
    val s = Node("s")
    val a = Node("a")
    val b = Node("b")
    val c = Node("c")
    val d = Node("d")
    val e = Node("e")

    val graph = UndirectedGraph(s,a,b,c,d,e)
    graph addEdges((s, a),
      (s, b),
      (a, c),
      (b, c),
      (b, d),
      (c, d),
      (c, e),
      (d, e))

    graph.getOutgoingNodes(s) should contain theSameElementsAs Seq(a,b)
    graph.getOutgoingNodes(a) should contain theSameElementsAs Seq(s,c)
    graph.getOutgoingNodes(b) should contain theSameElementsAs Seq(s,c,d)
    graph.getOutgoingNodes(c) should contain theSameElementsAs Seq(a,b,e,d)
    graph.getOutgoingNodes(d) should contain theSameElementsAs Seq(b, c,e)
    graph.getOutgoingNodes(e) should contain theSameElementsAs Seq(c,d)

  }

  it should "do bfs for undirected edges" in {
    val s = Node("s")
    val a = Node("a")
    val b = Node("b")
    val c = Node("c")
    val d = Node("d")
    val e = Node("e")

    val graph = UndirectedGraph(s,a,b,c,d,e)
    graph addEdges((s, a),
      (s, b),
      (a, c),
      (b, c),
      (b, d),
      (c, d),
      (c, e),
      (d, e))

    graph.bfs(s) should equal(Set(s, a, b, c, d, e))
  }

  it should "do dfs for undirected edges" in {
    val s = Node("s")
    val a = Node("a")
    val b = Node("b")
    val c = Node("c")
    val d = Node("d")
    val e = Node("e")

    val graph = UndirectedGraph(s,a,b,c,d,e)
    graph addEdges((s, a),
      (s, b),
      (a, c),
      (b, c),
      (b, d),
      (c, d),
      (c, e),
      (d, e))

    graph.dfs(s) should equal(List(s, b, d, e, c, a))
  }

}
