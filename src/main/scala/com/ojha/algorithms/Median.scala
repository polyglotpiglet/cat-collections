package com.ojha.algorithms

import com.ojha.mutable.Heap

object Median {

  def computeMedian(numbers: Seq[Int]): Int = {
    val maxHeap = Heap.empty[Int]
    val minHeap = Heap.empty[Int](Ordering[Int].reverse)

    def populatedHeaps(numbers: Seq[Int]): (Heap[Int], Heap[Int]) = {
      numbers match {
        case Nil => (maxHeap, minHeap)
        case n::ns if maxHeap.isEmpty && minHeap.isEmpty => maxHeap.insert(n); populatedHeaps(ns)
        case n::ns if minHeap.isEmpty => {
          val lowerMax = maxHeap.top
          if (n > lowerMax) minHeap.insert(n)
          else { minHeap.insert(maxHeap.popTop()) ; maxHeap.insert(n) }
          populatedHeaps(ns)
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
          populatedHeaps(ns)
        }
      }
    }
    populatedHeaps(numbers.toList)

    if (maxHeap.size > minHeap.size) maxHeap.top
    else if (maxHeap.size < minHeap.size) minHeap.top
    else (maxHeap.top + minHeap.top) / 2

  }

}
