package com.ojha.algorithms

import com.ojha.mutable.Heap

object Median {

  /**
    * @param numbers (stream of n numbers)
    * @return (median(n_1), median(n_1, n_2), ... , median(n_1,...,n_n))
    */
  def computeAllMedian(numbers: Seq[Int]): Seq[Int] = {
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
    go(numbers.toList, List.empty).reverse
  }

  /**
    * @param maxHeap
    * @param minHeap
    * @return
    */
  private def computeMedian(maxHeap: Heap[Int], minHeap: Heap[Int]): Int = {
    if (maxHeap.size == minHeap.size) maxHeap.top
    else if (minHeap.size > maxHeap.size) minHeap.top
    else maxHeap.top
  }

  def computeMedian(numbers: Seq[Int]): Int = computeAllMedian(numbers).last

}
