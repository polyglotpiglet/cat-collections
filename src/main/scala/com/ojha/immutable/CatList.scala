package com.ojha.immutable

import scala.annotation.tailrec

sealed trait CatList[+A] {
  def tail: CatList[A]
  def drop(n: Int): CatList[A]
  def size: Int
  def dropWhile(f: A => Boolean): CatList[A]
  def foldRight[B](b: B, f: (A,B) => B): B
  def foldLeft[B](b: B, f: (B,A) => B): B
  def map[B](f: A => B): CatList[B]
  def flatMap[B](f: A => CatList[B]): CatList[B]
  def init: CatList[A]
}
case object Nil extends CatList[Nothing] {
  override def tail: CatList[Nothing] = throw new RuntimeException("Tail of an empty CatList!")

  override def drop(n: Int): CatList[Nothing] = {
    if (n == 0) Nil
    else throw new RuntimeException("Can't drop from empty list!")
  }

  override def size: Int = 0

  override def init = throw new RuntimeException("Init of an empty CatList!")

  override def dropWhile(f: (Nothing) => Boolean) = Nil

  override def foldRight[B](b: B, f: (Nothing, B) => B): B = b

  override def foldLeft[B](b: B, f: (B, Nothing) => B): B = b

  override def map[B](f: (Nothing) => B): CatList[B] = Nil

  override def flatMap[B](f: (Nothing) => CatList[B]): CatList[B] = Nil
}
case class Cons[+A](head: A, tail: CatList[A]) extends CatList[A] {
  override def drop(n: Int): CatList[A] = {
    if (n == 0) this
    else tail.drop(n-1)
  }

  override def size: Int = foldRight[Int](0, (_,b) => b + 1)

  override def init: CatList[A] = {
    def aux(l: CatList[A], acc: CatList[A]): CatList[A] = l match {
      case Cons(_, Nil) => acc
      case Cons(h, t) => aux(t, Cons(h, acc))
    }
    aux(this, Nil)
  }

  override def dropWhile(f: A => Boolean): CatList[A] = {
    if (f(head)) tail.dropWhile(f)
    else this
  }

  override def foldRight[B](b: B, f: (A, B) => B): B = {
    foldLeft[B](b, (b,a) => f(a,b))
    //    tail.foldRight(f(head, b), f) // naughty non safe
  }

  override def foldLeft[B](b: B, f: (B, A) => B): B = {
    @tailrec
    def aux(ls: CatList[A], b: B, res: CatList[A] = Nil): B = ls match {
      case Nil => b
      case Cons(h, ts) => aux(ts, f(b, h), Cons(h, res))
    }
    aux(this, b)
  }

  override def map[B](f: (A) => B): CatList[B] = {
    Cons(f(head), tail.map(f))
  }

  override def flatMap[B](f: (A) => CatList[B]): CatList[B] = ???
}

object CatList {

  def apply[A](as: A*): CatList[A] = {
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))
  }

}
