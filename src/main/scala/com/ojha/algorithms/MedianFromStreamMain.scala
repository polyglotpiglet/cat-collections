package com.ojha.algorithms

import scala.io.Source

object MedianFromStreamMain extends App {

  val reader = Source.fromResource("dataStreamForFindMedian.txt").getLines.buffered
  val numbers = reader.map(_.toInt).toSeq
  val r = Median.computeAllMedian(numbers)
  println(r.map(BigInt(_)).sum % 10000)
}
