package com.ojha.mutable.graphs

import org.scalatest.{FlatSpec, Matchers}

class DirectedGraphSuite extends FlatSpec with Matchers {

  it should "correctly return outgoing connections for directed edges" in {
    val s = Node("s")
    val a = Node("a")
    val b = Node("b")
    val c = Node("c")
    val d = Node("d")
    val e = Node("e")

    val graph = DirectedGraph(s,a,b,c,d,e)
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

  it should "do bfs for directed edges" in {
    val s = Node("s")
    val a = Node("a")
    val b = Node("b")
    val c = Node("c")
    val d = Node("d")
    val e = Node("e")

    val graph = DirectedGraph(s,a,b,c,d,e)
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

    val graph = DirectedGraph(s,a,b,c,d,e)
    graph addEdges((s, a),
      (s, b),
      (a, c),
      (b, c),
      (b, d),
      (c, d),
      (c, e),
      (d, e))

    graph.dfs(s) should equal(Set(s, a, c, e, d, b))
  }

  it should "verify topological sort for graph" in {
    val a = Node("a")
    val b = Node("b")
    val c = Node("c")
    val d = Node("d")

    val graph = DirectedGraph(a,b,c,d)
    graph addEdges((a, b),
      (a, c),
      (b, d),
      (c, d))

    graph.isTopologicalSort(Set(a,b,c,d)) should be(true)
    graph.isTopologicalSort(Set(a,c,b,d)) should be(true)
    graph.isTopologicalSort(Set(a,b,c)) should be(false)
    graph.isTopologicalSort(Set(c,a,d,b)) should be(false)
  }

}
