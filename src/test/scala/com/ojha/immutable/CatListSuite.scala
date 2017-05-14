package com.ojha.immutable

import com.ojha.immutable
import org.scalatest.{FlatSpec, Matchers}

class CatListSuite extends FlatSpec with Matchers {
  it should "correctly compute size" in {
    CatList.size(CatList()) should be(0)
    CatList.size(CatList("A")) should be(1)
    CatList.size(CatList("A", "B", "C", "D")) should be(4)
  }

  it should "allow you to drop from a list" in {
    CatList.drop(CatList("A"), 1) should be(Nil)
    CatList.drop(CatList("A", "B", "C", "D"), 2) should be(CatList("C", "D"))
  }

  it should "allow you to dropWhile a predicate is true" in {
    CatList.dropWhile[String](CatList("A"), a => a == "A") should be(Nil)
    CatList.dropWhile[String](CatList("A", "B", "C", "D"), _ != "C") should be(CatList("C", "D"))
  }

  it should "removes the last element of the list" in {
    CatList.init(CatList("A")) should be(Nil)
    CatList.init(CatList("A", "B", "C", "D")) should be(CatList("A", "B", "C"))
  }

  it should "folds should associate correctly" in {
    CatList.foldRight[Int, Int](CatList(1, 2), 0, _ - _) should be(-1)
    CatList.foldLeft[Int, Int](CatList(1, 2), 0, _ - _) should be(-3)
  }

  it should "map" in {
    CatList.map[Int, Int](CatList(1, 2), _ * 2) should be(CatList(2, 4))
    CatList.map[Int, String](CatList(1, 2), _ + " ") should be(CatList("1 ", "2 "))
  }

  it should "append" in {
    CatList.append(CatList(1, 2), CatList(3, 4)) should be(CatList(1, 2, 3, 4))
  }

  it should "flatMap" in {
    CatList.flatMap[Int, Int](CatList(1, 2), i => CatList(i, i)) should be(CatList(1, 1, 2, 2))
  }

  it should "filter" in {
    CatList.filter[Int](CatList(1, 2, 3, 4, 1, 0, 5), _ > 1) should be(CatList(2, 3, 4, 5))
  }

  it should "zipwith" in {
    CatList.zipWith[Int, Int, Int](CatList(1, 2), CatList(3, 4), _ + _) should be(CatList(4, 6))
  }

  it should "report if a sublist is contained in a list" in {
    CatList.hasSubsequence(CatList(1,2), CatList(1)) should be (true)
    CatList.hasSubsequence(CatList(1,2), CatList(1,2)) should be (true)
    CatList.hasSubsequence(CatList(1,2), CatList(1,2,3)) should be (false)
    CatList.hasSubsequence(CatList(1,1,1,2,3), CatList(1,2)) should be (true)
  }

}
