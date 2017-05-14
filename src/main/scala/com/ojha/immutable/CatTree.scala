package com.ojha.immutable

sealed trait CatTree[+A]

case class Leaf[A](value: A) extends CatTree[A]

case class Branch[A](value: A, left: CatTree[A], right: CatTree[A]) extends CatTree[A]

object CatTree {

  def size[A](tree: CatTree[A]): Int = tree match {
    case Leaf(_) => 1
    case Branch(_, l, r) => size(l) + size(r) + 1
  }

}


