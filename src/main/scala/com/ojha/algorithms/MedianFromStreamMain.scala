package com.ojha.algorithms

import com.ojha.mutable.Heap

import scala.io.Source

object MedianFromStreamMain extends App {

  val reader = Source.fromResource("dataStreamForFindMedian.txt").getLines.buffered
  val numbers = reader.map(_.toInt).toSeq
  val r = computeMedian(numbers)
  println(r.map(BigInt(_)).sum % 10000)


  def computeMedian(numbers: Seq[Int]): Seq[Int] = {
    val maxHeap = Heap.empty[Int]
    val minHeap = Heap.empty[Int](Ordering[Int].reverse)

    def go(numbers: Seq[Int], result: List[Int]): Seq[Int] = {
      numbers match {
        case Nil => result
        case n::ns if maxHeap.isEmpty && minHeap.isEmpty => maxHeap.insert(n); go(ns, computeMedian(maxHeap, minHeap) ::result)
        case n::ns if minHeap.isEmpty => {
          val lowerMax = maxHeap.top
          if (n > lowerMax) minHeap.insert(n)
          else { minHeap.insert(maxHeap.popTop()) ; maxHeap.insert(n) }
          go(ns, computeMedian(maxHeap, minHeap) :: result)
        }
        case n::ns => {
          if (maxHeap.top >= n) {
            maxHeap.insert(n)
            while (maxHeap.size > minHeap.size + 1) minHeap.insert(maxHeap.popTop())
          }
          else {
            minHeap.insert(n)
            while (minHeap.size > maxHeap.size + 1) maxHeap.insert(minHeap.popTop())
          }
          go(ns, computeMedian(maxHeap, minHeap) :: result)
        }
      }
    }
    go(numbers.toList, List.empty)
  }

  def computeMedian(maxHeap: Heap[Int], minHeap: Heap[Int]): Int = {
    if (maxHeap.size == minHeap.size) maxHeap.top
    else if (minHeap.size > maxHeap.size) minHeap.top
    else maxHeap.size
  }

}
