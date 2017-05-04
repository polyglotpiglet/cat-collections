package com.ojha.mutable.maps

trait Map[K,V] {

  def get(key: K): V

  def put(key: K, value: V): Unit

  def size: Int

  def isEmpty: Boolean

  def nonEmpty: Boolean

}
