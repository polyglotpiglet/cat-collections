package com.ojha.algorithms

import org.scalatest.{FlatSpec, Matchers}

class MedianSuite extends FlatSpec with Matchers {

  it should "compute median" in {
    Median.computeMedian(Seq(1,3,5,4,2)) should be (3)
    Median.computeMedian(Seq(1,3,5,6)) should be (4)
    Median.computeMedian(Seq(1)) should be (1)
  }

}
