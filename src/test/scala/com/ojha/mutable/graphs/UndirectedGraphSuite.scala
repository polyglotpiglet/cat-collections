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

    graph.preOrderDfs(s) should equal(List(s, b, d, e, c, a))
  }

  it should "compute shortest path between two nodes in an undirected connected graph" in {

    /*         e
              / \
    s - a - c  - d
      \    /    /
        b  - - -
     */

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

    graph.shortestPath(s,a) should equal(List(s,a))
    graph.shortestPath(s,b) should equal(List(s,b))
    graph.shortestPath(s,d) should equal(List(s,b,d))
    Seq(Seq(s,b,c), Seq(s,a,c)) should contain(graph.shortestPath(s,c)) // 2 options for shortest path to c
    Seq(Seq(s,b,c,e), Seq(s,a,c,e), Seq(s,b,d,e)) should contain(graph.shortestPath(s,e)) // 3 options for shortest path to e
  }

  it should "compute the diameter of an undirected connected graph" in {

    /*         e
              / \
    s - a - c  - d
      \    /    /
        b  - - -
     */

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

    graph.diameter should equal(4)
  }

  it should "compute the radius of an undirected connected graph" in {

    /*         e
              / \
    s - a - c  - d
      \    /    /
        b  - - -
     */

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

    graph.radius should equal(3)
  }

  it should "compute the diameter of an 4 node cyclic undirected connected graph" in {
    /*

    s - a - c
      \    /
        b
     */

    val s = Node("s")
    val a = Node("a")
    val b = Node("b")
    val c = Node("c")

    val graph = UndirectedGraph(s,a,b,c)
    graph addEdges(
      (s, a),
      (s, b),
      (a, c),
      (b, c))

    graph.diameter should equal(3)
  }

  it should "compute the radius of an 4 node cyclic undirected connected graph" in {
    /*

    s - a - c
      \    /
        b
     */

    val s = Node("s")
    val a = Node("a")
    val b = Node("b")
    val c = Node("c")

    val graph = UndirectedGraph(s,a,b,c)
    graph addEdges(
      (s, a),
      (s, b),
      (a, c),
      (b, c))

    graph.radius should equal(3)
  }

  it should "do postorder dfs" in {

    /*         e
              / \
    s - a - c  - d
      \    /    /
        b  - - -
     */

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

    graph.postOrderDfs(s) should equal(List(e, d, c, b, a, s))
  }

}
