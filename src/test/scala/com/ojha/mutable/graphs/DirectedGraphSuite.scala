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

  it should "do dfs for directed edges" in {
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

    graph.dfs(s) should equal(List(s, b, d, e, c, a))
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

    graph.isTopologicalSort(List(a,b,c,d)) should be(true)
    graph.isTopologicalSort(List(a,c,b,d)) should be(true)
    graph.isTopologicalSort(List(a,b,c)) should be(false)
    graph.isTopologicalSort(List(c,a,d,b)) should be(false)
  }

  it should "generate topological sort for graph" in {
    val a = Node("a")
    val b = Node("b")
    val c = Node("c")
    val d = Node("d")

    val graph = DirectedGraph(a,b,c,d)
    graph addEdges((a, b),
      (a, c),
      (b, d),
      (c, d))

    val topSort = graph.topologicalSort
    graph.isTopologicalSort(topSort) should be(true)
  }

  it should "fail to generate topological sort for directed cyclic graph" in {
    val a = Node("a")
    val b = Node("b")
    val c = Node("c")
    val d = Node("d")

    val graph = DirectedGraph(a,b,c,d)
    graph addEdges((a, b),
      (b, c),
      (d, d),
      (d, a))

    val topSort = graph.topologicalSort
    graph.isTopologicalSort(topSort) should be(false)
  }

}
