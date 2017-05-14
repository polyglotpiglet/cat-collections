package com.ojha.immutable

import scala.annotation.tailrec

sealed trait CatList[+A]
case object Nil extends CatList[Nothing]
case class Cons[+A](head: A, tail: CatList[A]) extends CatList[A]

object CatList {

  def drop[A](as: CatList[A], n: Int): CatList[A] = as match {
    case _ if n == 0 => as
    case Nil => throw new RuntimeException
    case Cons(_, t) => drop(t, n- 1)
  }

  def size[A](as: CatList[A]): Int = as match {
    case Nil => 0
    case Cons(_, t) => 1 + size(t)
  }

  def dropWhile[A](as: CatList[A], f: A => Boolean): CatList[A] = as match {
    case Nil => Nil
    case Cons(h, t) => if (f(h)) dropWhile(t, f) else as
  }
  def foldRight[A,B](as: CatList[A], b: B, f: (A,B) => B): B = as match {
    case Nil => b
    case Cons(h,t) => f(h, foldRight(t, b, f))
  }

  @tailrec
  def foldLeft[A, B](as: CatList[A], b: B, f: (B,A) => B): B = as match {
    case Nil => b
    case Cons(h,t) => foldLeft(t, f(b,h), f)
  }

  def map[A, B](as: CatList[A], f: A => B): CatList[B] = as match {
    case Nil => Nil
    case Cons(h, t) => Cons(f(h), map(t, f))
  }

  def flatMap[A, B](as: CatList[A], f: A => CatList[B]): CatList[B] = {
    foldLeft(as, Nil:CatList[B], (b:CatList[B], a: A) => append[B](b, f(a)))
  }

  def init[A](as: CatList[A]): CatList[A] = as match {
    case Nil => throw new RuntimeException
    case Cons(_, Nil) => Nil
    case Cons(h, t) => Cons(h, init(t))
  }
  def append[A](as: CatList[A], bs: CatList[A]): CatList[A] = {
    foldRight[A, CatList[A]](as, bs, Cons[A])
  }

  def tail[A](as: CatList[A]): CatList[A] = as match {
    case Nil => throw new RuntimeException("tail of empty list")
    case Cons(_, t) => t
  }

  def filter[A](as: CatList[A], f: A => Boolean): CatList[A] = as match {
    case Nil => Nil
    case Cons(h, t) if f(h) => Cons(h, filter(t, f))
    case Cons(_, t) => filter(t, f)
  }

  def zipWith[A,B,C](as: CatList[A], bs: CatList[B], f: (A,B) => C): CatList[C] = (as,bs) match {
    case (Nil, Nil) => Nil
    case (Nil, _) => throw new RuntimeException("zippy lists must be the same length")
    case (_, Nil) => throw new RuntimeException("zippy lists must be the same length")
    case (Cons(ha, ta), Cons(hb, tb)) => Cons(f(ha, hb), zipWith(ta, tb, f))
  }

  def hasSubsequence[A](as: CatList[A], ls: CatList[A]): Boolean = (as, ls) match {
        case (_, Nil) => true
        case (Nil, _) => false
        case (Cons(h1, t1), Cons(h2, t2)) if h1 == h2 => hasSubsequence(t1, t2) || hasSubsequence(t1, ls)
        case (Cons(_, t1), _) => hasSubsequence(t1, ls)
  }

  def apply[A](as: A*): CatList[A] = {
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))
  }

}
