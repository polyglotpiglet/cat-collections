package com.ojha.mutable

import org.scalatest.{FlatSpec, Matchers}

class HeapSuite extends FlatSpec with Matchers {

  it should "calculate child index" in {
    val sut = new Heap[Int]
    sut.childIndex(0) should be(1)
    sut.childIndex(1) should be(3)
    sut.childIndex(2) should be(5)
    sut.childIndex(3) should be(7)
  }

  it should "calculate parent index" in {
    val sut = new Heap[Int]
    sut.parentIndex(1) should be(0)
    sut.parentIndex(3) should be(1)
    sut.parentIndex(5) should be(2)
    sut.parentIndex(7) should be(3)
  }

  it should "insert one" in {
    val sut = new Heap[Int]
    sut.insert(2)
    sut.top should be(2)
  }

  it should "insert lots" in {
    implicit val ord = Ordering[Int].reverse
    val sut = new Heap[Int]
    sut.insert(2)
    sut.insert(1)
    sut.insert(3)
    sut.insert(4)
    sut.insert(7)
    sut.insert(6)
    sut.insert(5)
    sut.top should be(1)
  }

  it should "insert lots into reverse heap" in {
    val sut = new Heap[Int]
    sut.insert(2)
    sut.insert(1)
    sut.insert(3)
    sut.insert(4)
    sut.insert(7)
    sut.insert(6)
    sut.insert(5)
    sut.top should be(7)
  }

  it should "pop one top" in {
    val sut = new Heap[Int]
    sut.insert(2)
    sut.top should be(2)
  }

  it should "delete by value" in {
    val sut = new Heap[Int]
    sut.insert(2)
    sut.insert(7)
    sut.insert(4)
    sut.insert(3)
    sut.deleteValue(7)
    sut.top should be(4)
  }

  it should "delete multiple by value" in {
    val sut = new Heap[Int]
    sut.insert(2)
    sut.insert(7)
    sut.insert(4)
    sut.insert(3)
    sut.deleteValue(7)
    sut.deleteValue(4)
    sut.deleteValue(3)
    sut.top should be(2)
  }

  it should "remove top" in {
    val sut = new Heap[Int]
    sut.insert(2)
    sut.insert(1)
    sut.insert(3)
    sut.insert(4)
    sut.insert(7)
    sut.insert(6)
    sut.insert(5)
    sut.popTop should be(7)
    sut.popTop should be(6)
    sut.popTop should be(5)
    sut.popTop should be(4)
    sut.popTop should be(3)
    sut.popTop should be(2)
    sut.popTop should be(1)
  }

  it should "pretty print heap" in {
    val sut = new Heap[Int]
    sut.insert(2)
    sut.print should equal("2")
  }

}
