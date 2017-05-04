package com.ojha.immutable

import org.scalatest.{FlatSpec, Matchers}

class CatListSuite extends FlatSpec with Matchers {
  it should "correctly compute size" in {
    CatList().size should be(0)
    CatList("A").size should be(1)
    CatList("A", "B", "C", "D").size should be(4)
  }

  it should "allow you to drop from a list" in {
    CatList("A").drop(1).size should be(0)
    CatList("A", "B", "C", "D").drop(2).size should be(2)
  }

  it should "allow you to dropWhile a predicate is true" in {
    CatList("A").dropWhile(_ == "A").size should be(0)
    CatList("A", "B", "C", "D").dropWhile(_ != "C").size should be(2)
  }

  it should "removes the last element of the list" in {
    CatList("A").init.size should be(0)
    CatList("A", "B", "C", "D").init.size should be(3)
  }

  it should "compute sum of an int list using fold right" in {
    CatList(1, 2, 3, 4).foldRight[Int](0, _ + _) should be(10)
  }

  it should "fold right associates to the right" in {
    CatList(1, 2).foldRight[Int](0, _ - _) should be(1)
  }

  it should "compute product of an int list using fold left" in {
    CatList(1, 2, 3).foldLeft[Int](1, _ * _) should be(6)
  }

  it should "compute sum of an int list using fold left" in {
    CatList(1, 2, 3, 4).foldLeft[Int](0, _ + _) should be(10)
  }

  it should "compute product of an int list using fold right" in {
    CatList(1, 2, 3).foldRight[Int](1, _ * _) should be(6)
  }

  it should "reverse a list using fold right" in {
    CatList(1, 2, 3).foldRight[CatList[Int]](Nil: CatList[Int], Cons(_, _)) should be(CatList(3, 2, 1))
  }

  it should "fold left associates to the left" in {
    CatList(1, 2).foldLeft[Int](0, _ - _) should be(-3)
  }

  it should "map" in {
    CatList(1, 2).map[Int](_ * 2) should be(CatList(2,4))
    CatList(1, 2).map[String](_ + " ") should be(CatList("1 ", "2 "))
  }

}
