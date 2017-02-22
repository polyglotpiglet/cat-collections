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

    graph.shortestPath(s, a) should equal(List(s, a))
    graph.shortestPath(s, b) should equal(List(s, b))
    graph.shortestPath(s, d) should equal(List(s, b, d))
    Seq(Seq(s, b, c), Seq(s, a, c)) should contain(graph.shortestPath(s, c)) // 2 options for shortest path to c
    Seq(Seq(s, b, c, e), Seq(s, a, c, e), Seq(s, b, d, e)) should contain(graph.shortestPath(s, e)) // 3 options for shortest path to e
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

  it should "do partial pre order dfs" in {
    val graph = new DirectedGraph[Int]()
    val nodes = (1 to 12).map(Node(_))
    nodes.foreach(graph.addNode)

    val edgeString = "1,2 2,3 2,4 2,5 3,6 4,5 4,7 5,2 5,6 5,7 6,3 6,8 7,8 7,10 8,7 9,7 10,9 10,11 11,12 12,10"
    val edgePairs = edgeString.split(" ").map(_.split(",")).map(a => (Node(a(0).toInt), Node(a(1).toInt)))
    edgePairs.foreach(graph.addUnitEdge)

    graph.preOrderDfs(Node(12)).map(_.value) should equal(Seq(12, 10, 11, 9, 7, 8))
  }



  it should "find five biggest sccs in graph" in {
    val graph = new DirectedGraph[Int]()
    val nodes = (1 to 12).map(Node(_))
    nodes.foreach(graph.addNode)

    val edgeString = "1,2 2,3 2,4 2,5 3,6 4,5 4,7 5,2 5,6 5,7 6,3 6,8 7,8 7,10 8,7 9,7 10,9 10,11 11,12 12,10"
    val edgePairs = edgeString.split(" ").map(_.split(",")).map(a => (Node(a(0).toInt), Node(a(1).toInt)))
    edgePairs.foreach(graph.addUnitEdge)

    graph.stronglyConnected.map(_.size).sorted.reverse.take(5) should equal(Seq(6, 3, 2, 1))
  }

  it should "find five biggest sccs in 8 node example graph" in {
    val graph = new DirectedGraph[Int]()
    val nodes = (1 to 8).map(Node(_))
    nodes.foreach(graph.addNode)
    val edgeString = "1,2 2,3 3,1 3,4 5,4 6,4 8,6 6,7 7,8 4,3 4,6"
    val edgePairs = edgeString.split(" ").map(_.split(",")).map(a => (Node(a(0).toInt), Node(a(1).toInt)))
    edgePairs.foreach(graph.addUnitEdge)

    graph.stronglyConnected.map(_.size).sorted.reverse.take(5) should equal(Seq(7, 1))
  }

  it should "find five biggest sccs in 9 node example graph" in {
    val edgeString = "1,4 2,8 3,6 4,7 5,2 6,9 7,1 8,5 8,6 9,7 9,3"

    val graph = new DirectedGraph[Int]()
    val nodes = (1 to 9).map(Node(_))
    nodes.foreach(graph.addNode)
    val edgePairs = edgeString.split(" ").map(_.split(",")).map(a => (Node(a(0).toInt), Node(a(1).toInt)))
    edgePairs.foreach(graph.addUnitEdge)

    graph.stronglyConnected.map(_.size).sorted.reverse.take(5) should equal(Seq(3, 3, 3))
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

    /*
       c
     ^   \
    /     v
  a        d
    \     ^
     v   /
       b
   */

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

    /*
         c
       ^   \
      /     v
    a        d
      ^     /
       \   v
         b
     */

    val a = Node("a")
    val b = Node("b")
    val c = Node("c")
    val d = Node("d")

    val graph = DirectedGraph(a, b, c, d)
    graph addEdges((a, b),
      (b, c),
      (c, d),
      (d, a))

    val topSort = graph.topologicalSort
    graph.isTopologicalSort(topSort) should be(false)
  }

  it should "be a cat" in {

    /*
           b
         ^    \
        /      \
       /   |    v
    a  < - | - - d
      \    |    ^
       \   |   /
        v  v  /
           c
     */

    val a = Node("a")
    val b = Node("b")
    val c = Node("c")
    val d = Node("d")

    val graph = DirectedGraph(a, b, c, d)
    graph addEdges((a, b),
      (b, d),
      (b, c),
      (d, a),
      (c, d),
      (a, c))

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
    //    graph.reverse()

    val actual = graph.stronglyConnected.map(_.sortWith((a, b) => a.value < b.value))
    actual.size should be(3)
    actual should contain(Seq(a, d, g))
    actual should contain(Seq(c, f, i))
    actual should contain(Seq(b, e, h))

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
    //    graph.reverse()

    val actual = graph.stronglyConnected.map(_.sortWith((a, b) => a.value < b.value))
    actual.size should be(2)
    actual should contain(Seq(b, e, h))
    actual should contain(Seq(a, c, d, f, g, i))

  }

  it should "compute strong connected components for another graph with 10 nodes" in {
    val a = Node("a")
    val b = Node("b")
    val c = Node("c")
    val d = Node("d")
    val e = Node("e")
    val f = Node("f")
    val g = Node("g")
    val h = Node("h")
    val i = Node("i")
    val j = Node("j")
    val k = Node("k")

    val graph = DirectedGraph(a, b, c, d, e, f, g, h, i, j, k)
    graph.addEdges((a, b), (b, c), (c, a), (b, d), (d, e), (e, f), (f, d), (g, f), (g, h), (h, i), (i, j), (j, g), (j, k))

    val components = graph.stronglyConnected.map(_.sortWith((a, b) => a.value < b.value))
    components.size should equal(4)
    components should contain(Seq(k))
    components should contain(Seq(a, b, c))
    components should contain(Seq(d, e, f))
    components should contain(Seq(g, h, i, j))
  }

  it should "compute shortest path between nodes in directed graph where nodes have weighted edges" in {

    /*
            1      6
        s - -> v - - > t
          \    |      ^
        4  \   | 2   / 3
            v  v    /
               w -
         */

    val s = Node("s")
    val v = Node("v")
    val w = Node("w")
    val t = Node("t")

    val graph = DirectedGraph(s, v, w, t)
    graph addWeightedEdges(
      (s, v, 1),
      (s, w, 4),
      (v, w, 2),
      (w, t, 3),
      (v, t, 6))

    graph.shortestWeightedPath(s, s) should equal((List(s), 0))
    graph.shortestWeightedPath(s, v) should equal((List(s, v), 1))
    graph.shortestWeightedPath(s, w) should equal((List(s, v, w), 3))
    graph.shortestWeightedPath(s, t) should equal((List(s, v, w, t), 6))
    graph.shortestWeightedPath(t, v) should equal((List(), Int.MaxValue))
  }

  it should "compute shortest paths too all other nodes  in directed graph where nodes have weighted edges" in {

    /*
            1      6
        s - -> v - - > t
          \    |      ^
        4  \   | 2   / 3
            v  v    /
               w -
         */

    val s = Node("s")
    val v = Node("v")
    val w = Node("w")
    val t = Node("t")

    val graph = DirectedGraph(s, v, w, t)
    graph addWeightedEdges(
      (s, v, 1),
      (s, w, 4),
      (v, w, 2),
      (w, t, 3),
      (v, t, 6))

    val result = graph.shortestWeightedPathsToAllOtherNodes(s)

    result.size should be(4)
    result(s) should equal((List(s), 0))
    result(v) should equal((List(s, v), 1))
    result(w) should equal((List(s, v, w), 3))
    result(t) should equal((List(s, v, w, t), 6))
  }
}
