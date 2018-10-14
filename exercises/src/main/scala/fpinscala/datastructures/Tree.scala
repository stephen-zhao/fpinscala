package fpinscala.datastructures

sealed trait Tree[+A]
case class Leaf[A](value: A) extends Tree[A]
case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]


object Tree {

  def size[A](t: Tree[A]): Int = t match {
    case Leaf(_) => 1
    case Branch(l, r) => size(l) + 1 + size(r)
  }

  def maximum(t: Tree[Int]): Int = t match {
    case Leaf(v) => v
    case Branch(l, r) => maximum(l) max maximum(r)
  }

  def depth[A](t: Tree[A]): Int = t match {
    case Leaf(v) => 0
    case Branch(l, r) => (depth(l) max depth(r)) + 1
  }

  def map[A, B](t: Tree[A])(f: A => B): Tree[B] = t match {
    case Leaf(v) => Leaf(f(v))
    case Branch(l, r) => Branch(map(l)(f), map(r)(f))
  }

  def fold[A, B](t: Tree[A])(z: A => B)(f: (B, B) => B): B = t match {
    case Leaf(v) => z(v)
    case Branch(l, r) => f(fold(l)(z)(f), fold(r)(z)(f))
  }

  def sizeByFold[A](t: Tree[A]): Int = {
    fold[A, Int](t)(_ => 1)(_ + 1 + _)
  }

  def maximumByFold(t: Tree[Int]): Int = {
    fold[Int, Int](t)(v => v)(_ max _)
  }

  def depthByFold[A](t: Tree[A]): Int = {
    fold[A, Int](t)(_ => 1)((l, r) => (l max r) + 1)
  }

  def mapByFold[A, B](t: Tree[A])(f: A => B): Tree[B] = {
    fold[A, Tree[B]](t)(v => Leaf(f(v)))((l, r) => Branch(l, r))
  }



}