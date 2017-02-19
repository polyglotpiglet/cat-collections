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

    val graph = DirectedGraph(s, a, b, c, d, e)
    graph addEdges((s, a),
      (s, b),
      (a, c),
      (b, c),
      (b, d),
      (c, d),
      (c, e),
      (d, e))

    graph.getOutgoingNodes(s) should contain theSameElementsAs Seq(a, b)
    graph.getOutgoingNodes(a) should contain theSameElementsAs Seq(c)
    graph.getOutgoingNodes(b) should contain theSameElementsAs Seq(c, d)
    graph.getOutgoingNodes(c) should contain theSameElementsAs Seq(e, d)
    graph.getOutgoingNodes(d) should contain theSameElementsAs Seq(e)
    graph.getOutgoingNodes(e) should contain theSameElementsAs Seq()
  }

  it should "correctly return incoming connections for directed edges" in {
    val s = Node("s")
    val a = Node("a")
    val b = Node("b")
    val c = Node("c")
    val d = Node("d")
    val e = Node("e")

    val graph = DirectedGraph(s, a, b, c, d, e)
    graph addEdges((s, a),
      (s, b),
      (a, c),
      (b, c),
      (b, d),
      (c, d),
      (c, e),
      (d, e))

    graph.getIncomingNodes(s) should contain theSameElementsAs Seq()
    graph.getIncomingNodes(a) should contain theSameElementsAs Seq(s)
    graph.getIncomingNodes(b) should contain theSameElementsAs Seq(s)
    graph.getIncomingNodes(c) should contain theSameElementsAs Seq(a, b)
    graph.getIncomingNodes(d) should contain theSameElementsAs Seq(b, c)
    graph.getIncomingNodes(e) should contain theSameElementsAs Seq(c, d)
  }

  it should "do bfs for directed edges" in {
    val s = Node("s")
    val a = Node("a")
    val b = Node("b")
    val c = Node("c")
    val d = Node("d")
    val e = Node("e")

    val graph = DirectedGraph(s, a, b, c, d, e)
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

  it should "do preorder dfs for directed edges" in {

    /*           e
               ^  ^
              /    \
    s -> a -> c  -> d
      \      ^      ^
       v    /      /
          b  - - -
     */

    val s = Node("s")
    val a = Node("a")
    val b = Node("b")
    val c = Node("c")
    val d = Node("d")
    val e = Node("e")

    val graph = DirectedGraph(s, a, b, c, d, e)
    graph addEdges((s, a),
      (s, b),
      (a, c),
      (b, c),
      (b, d),
      (c, d),
      (c, e),
      (d, e))

    graph.preOrderDfs(s) should equal(List(s, b, d, e, c, a))
  }

  it should "compute shortest path between nodes in directed graph" in {

    /*           e
               ^  ^
              /    \
    s -> a -> c  -> d
      \      ^      ^
       v    /      /
          b  - - -
     */

    val s = Node("s")
    val a = Node("a")
    val b = Node("b")
    val c = Node("c")
    val d = Node("d")
    val e = Node("e")

    val graph = DirectedGraph(s, a, b, c, d, e)
    graph addEdges((s, a),
      (s, b),
      (a, c),
      (b, c),
      (b, d),
      (c, d),
      (c, e),
      (d, e))

    graph.shortestPath(s,a) should equal(List(s,a))
    graph.shortestPath(s,b) should equal(List(s,b))
    graph.shortestPath(s,d) should equal(List(s,b,d))
    Seq(Seq(s,b,c), Seq(s,a,c)) should contain(graph.shortestPath(s,c)) // 2 options for shortest path to c
    Seq(Seq(s,b,c,e), Seq(s,a,c,e), Seq(s,b,d,e)) should contain(graph.shortestPath(s,e)) // 3 options for shortest path to e

  }

  it should "do postorder for directed edges" in {

    /*           e
               ^  ^
              /    \
    s -> a -> c  -> d
      \      ^      ^
       v    /      /
          b  - - -
     */

    val s = Node("s")
    val a = Node("a")
    val b = Node("b")
    val c = Node("c")
    val d = Node("d")
    val e = Node("e")

    val graph = DirectedGraph(s, a, b, c, d, e)
    graph addEdges((s, a),
      (s, b),
      (a, c),
      (b, c),
      (b, d),
      (c, d),
      (c, e),
      (d, e))

    graph.postOrderDfs(s) should equal(List(e, d, c, b, a, s))
  }

  it should "do dfs where not all nodes are reachable" in {

    /*
    a   ->  g   ->   i  ->  f    ->    h   ->   e
     ^     /          ^    /            ^      /
      \   v            \  v              \    v
        d                c                  b

     */

    val a = Node("a")
    val b = Node("b")
    val c = Node("c")
    val d = Node("d")
    val e = Node("e")
    val f = Node("f")
    val g = Node("g")
    val h = Node("h")
    val i = Node("i")

    val graph = DirectedGraph(a, b, c, d, e, f, g, h, i)
    graph addEdges(
      (a, g),
      (g, d),
      (d, a),
      (g, i),
      (i, f),
      (f, c),
      (c, i),
      (f, h),
      (h, e),
      (e, b),
      (b, h))

    graph.postOrderDfs(h) should equal(Seq(b, e, h))
    graph.postOrderDfs(i) should equal(Seq(b, e, h, c, f, i))
    graph.postOrderDfs(a) should equal(Seq(b, e, h, c, f, i, d, g, a))

  }

  it should "verify topological sort for graph" in {
    val a = Node("a")
    val b = Node("b")
    val c = Node("c")
    val d = Node("d")

    val graph = DirectedGraph(a, b, c, d)
    graph addEdges((a, b),
      (a, c),
      (b, d),
      (c, d))

    graph.isTopologicalSort(List(a, b, c, d)) should be(true)
    graph.isTopologicalSort(List(a, c, b, d)) should be(true)
    graph.isTopologicalSort(List(a, b, c)) should be(false)
    graph.isTopologicalSort(List(c, a, d, b)) should be(false)
  }

  it should "generate topological sort for graph" in {
    val a = Node("a")
    val b = Node("b")
    val c = Node("c")
    val d = Node("d")

    val graph = DirectedGraph(a, b, c, d)
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

    val graph = DirectedGraph(a, b, c, d)
    graph addEdges((a, b),
      (b, c),
      (d, d),
      (d, a))

    val topSort = graph.topologicalSort
    graph.isTopologicalSort(topSort) should be(false)
  }

  it should "compute strongly connected components" in {

    /*

    a   ->  g   ->   i  ->  f    ->    h   ->   e
     ^     /          ^    /            ^      /
      \   v            \  v              \    v
        d                c                  b

     */

    val a = Node("a")
    val b = Node("b")
    val c = Node("c")
    val d = Node("d")
    val e = Node("e")
    val f = Node("f")
    val g = Node("g")
    val h = Node("h")
    val i = Node("i")

    val graph = DirectedGraph(a, b, c, d, e, f, g, h, i)
    graph addEdges((a, g), (g, d), (d, a), (g, i), (i, f), (f, c), (c, i), (f, h), (h, e), (e, b), (b, h))
    graph.reverse()

    val expected = Seq(
      Seq(b, e, h),
      Seq(f, i, c),
      Seq(d, g, a)
    )

    graph.stronglyConnected should equal(expected)

  }

  it should "compute strongly connected components for slightly different graph" in {

    /*

    a   ->  g   ->   i  ->  f    ->    h   ->   e
     ^     /          ^    /            ^      /
      \   v            \  v              \    v
        d      <-        c                  b

     */

    val a = Node("a")
    val b = Node("b")
    val c = Node("c")
    val d = Node("d")
    val e = Node("e")
    val f = Node("f")
    val g = Node("g")
    val h = Node("h")
    val i = Node("i")

    val graph = DirectedGraph(a, b, c, d, e, f, g, h, i)
    graph addEdges((a, g), (g, d), (d, a), (g, i), (i, f), (f, c), (c, i), (f, h), (h, e), (e, b), (b, h), (c, d))
    graph.reverse()

    val expected = Seq(
      Seq(b, e, h),
      Seq(c ,f, i, a, d, g)
    )

    graph.stronglyConnected should equal(expected)

  }

}
