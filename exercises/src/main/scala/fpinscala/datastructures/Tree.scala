package fpinscala.datastructures

sealed trait Tree[+A]
case class Leaf[A](value: A) extends Tree[A]
case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]

object Tree {
  def size[A](t: Tree[A]): Int = t match {
    case Leaf(_)             => 1
    case Branch(left, right) => 1 + size(left) + size(right)
  }

  def maximum(t: Tree[Int]): Int = t match {
    case Leaf(value)         => value
    case Branch(left, right) => maximum(left) max maximum(right)
  }

  def depth[A](t: Tree[A]): Int = t match {
    case Leaf(_)             => 0
    case Branch(left, right) => (depth(left) + 1) max (depth(right) + 1)
  }

  def map[A, B](t: Tree[A])(f: A => B): Tree[B] = t match {
    case Leaf(value)         => Leaf(f(value))
    case Branch(left, right) => Branch(map(left)(f), map(right)(f))
  }

  def fold[A, B](t: Tree[A])(l: A => B)(b: (B, B) => B): B = t match {
    case Leaf(value)         => l(value)
    case Branch(left, right) => b(fold(left)(l)(b), fold(right)(l)(b))
  }

  def sizeViaFold[A](t: Tree[A]): Int = fold(t)(_ => 1)((l, r) => 1 + l + r)

  def maximumViaFold(t: Tree[Int]): Int = fold(t)(identity)(_ max _)

  def depthViaFold[A](t: Tree[A]): Int =
    fold(t)(_ => 0)((l, r) => (l + 1) max (r + 1))

  def mapViaFold[A, B](t: Tree[A])(f: A => B): Tree[B] =
    fold(t)(a => Leaf(f(a)): Tree[B])((l, r) => Branch(l, r))
}
