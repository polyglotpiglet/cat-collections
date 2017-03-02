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

  it should "insert one element" in {
    val tree = RedBlackTree.empty[Int]
    tree.insert(5)
    tree.isEmpty should be(false)
    tree.size should be(1)
  }

  it should "do inorder traversal " in {
    RedBlackTree.of(5,2).inOrder should contain inOrder (2,5)
    RedBlackTree.of(2,5).inOrder should contain inOrder (2,5)
    RedBlackTree.of(2,5,1,7,6,4,3).inOrder should contain inOrder (1,2,3,4,5,6,7)
  }

}
