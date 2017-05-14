package com.ojha.immutable

import org.scalatest.{FlatSpec, Matchers}

class CatTreeSuite extends FlatSpec with Matchers {

  it should "compute size of a tree" in {
    CatTree.size(Leaf(1)) should be(1)
    CatTree.size(Branch(2, Leaf(1), Branch(1, Leaf(3), Leaf(5)))) should be(5)

  }

}
