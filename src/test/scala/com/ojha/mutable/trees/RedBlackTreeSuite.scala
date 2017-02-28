package com.ojha.mutable.trees

import org.scalatest.{FlatSpec, Matchers}

class RedBlackTreeSuite extends FlatSpec with Matchers {

  it should "start with zero size" in {
    val tree = RedBlackTree.empty[Int]
    tree.size should be(0)
  }

  it should "start with being empty" in {
    val tree = RedBlackTree.empty[Int]
    tree.isEmpty should be(true)
  }

}
