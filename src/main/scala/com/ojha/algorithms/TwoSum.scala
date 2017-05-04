package com.ojha.algorithms

import java.util

import scala.collection.immutable.HashSet
import scala.io.Source

object TwoSum extends App {

//  import collection.JavaConverters._

  val reader = Source.fromResource("dataStreamForTwoSum.txt").getLines.buffered
  val numbers = reader.map(n => BigInt(n)).toSet
  println(numbers.size + " " + numbers.contains(0))
//  val numbers = HashSet(1,2,3,24).map(BigInt(_))
//  val numbers = HashSet(-3,-1,1,2,9,11,7,6,2).map(BigInt(_))

  println((-100 to 100).count(i => canFind(BigInt(i))))
//  println((-10000 to 10000).count(i => canFind(BigInt(i))))
//
//  // what if we have two of the same? we can meet new targets where i is target
  def canFind(target: BigInt): Boolean = {
    for (n <- numbers) {
      val diff: BigInt = target - n
      val cat: BigInt = n
      if (cat != diff && numbers.contains(diff)) return true
    }

    false

////    numbers.exists( i => i != target - i && numbers.contains(target - i))
  }


}
