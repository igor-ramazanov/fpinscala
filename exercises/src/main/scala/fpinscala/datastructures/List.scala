package fpinscala.datastructures

sealed trait List[+A] // `List` data type, parameterized on a type, `A`
case object Nil extends List[Nothing] {
  override def toString: String = "[]"
} // A `List` data constructor representing the empty list
/* Another data constructor, representing nonempty lists. Note that `tail` is another `List[A]`,
which may be `Nil` or another `Cons`.
 */
case class Cons[+A](head: A, tail: List[A]) extends List[A] {
  override def toString: String = s"$head:${tail.toString}"
}

object List { // `List` companion object. Contains functions for creating and working with lists.
  def sum(ints: List[Int]): Int =
    ints match { // A function that uses pattern matching to add up a list of integers
      case Nil => 0 // The sum of the empty list is 0.
      case Cons(x, xs) =>
        x + sum(xs) // The sum of a list starting with `x` is `x` plus the sum of the rest of the list.
    }

  def product(ds: List[Double]): Double = ds match {
    case Nil          => 1.0
    case Cons(0.0, _) => 0.0
    case Cons(x, xs)  => x * product(xs)
  }

  def apply[A](as: A*): List[A] = // Variadic function syntax
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))

  val x = List(1, 2, 3, 4, 5) match {
    case Cons(x, Cons(2, Cons(4, _)))          => x
    case Nil                                   => 42
    case Cons(x, Cons(y, Cons(3, Cons(4, _)))) => x + y
    case Cons(h, t)                            => h + sum(t)
    case _                                     => 101
  }

  def append[A](a1: List[A], a2: List[A]): List[A] =
    a1 match {
      case Nil        => a2
      case Cons(h, t) => Cons(h, append(t, a2))
    }

  def foldRight[A, B](as: List[A], z: B)(f: (A, B) => B): B = // Utility functions
    as match {
      case Nil         => z
      case Cons(x, xs) => f(x, foldRight(xs, z)(f))
    }

  def sum2(ns: List[Int]) =
    foldRight(ns, 0)((x, y) => x + y)

  def product2(ns: List[Double]) =
    foldRight(ns, 1.0)(_ * _) // `_ * _` is more concise notation for `(x,y) => x * y`; see sidebar

  def tail[A](l: List[A]): List[A] = l match {
    case Nil           => Nil
    case Cons(_, tail) => tail
  }

  def setHead[A](l: List[A], h: A): List[A] = l match {
    case Nil           => Cons(h, Nil)
    case Cons(_, tail) => Cons(h, tail)
  }

  @annotation.tailrec
  def drop[A](l: List[A], n: Int): List[A] =
    if (n == 0)
      l
    else
      drop(tail(l), n - 1)

  @annotation.tailrec
  def dropWhile[A](l: List[A], f: A => Boolean): List[A] = l match {
    case Nil                  => Nil
    case c @ Cons(head, tail) => if (f(head)) dropWhile(tail, f) else c
  }

  def init[A](l: List[A]): List[A] = l match {
    case Cons(_, Nil)     => Nil
    case Cons(head, tail) => Cons(head, init(tail))
    case Nil              => Nil
  }

  def length[A](l: List[A]): Int = foldRight(l, 0) { (_, acc) =>
    acc + 1
  }

  @annotation.tailrec
  def foldLeft[A, B](l: List[A], z: B)(f: (B, A) => B): B =
    l match {
      case Nil              => z
      case Cons(head, tail) => foldLeft(tail, f(z, head))(f)
    }

  def sumViaFoldLeft[A](l: List[Int]): Int =
    foldLeft(l, 0)((acc, next) => acc + next)

  def productViaFoldLeft[A](l: List[Int]): Int =
    foldLeft(l, 1)((acc, next) => acc * next)

  def reverse[A](l: List[A]): List[A] =
    foldLeft(l, Nil: List[A])((acc, next) => Cons(next, acc))

  def foldLeftViaFoldRight[A, B](l: List[A], z: B)(f: (B, A) => B): B = {
    List.foldRight(l, (b: B) => b)((next, ff) => b => ff(f(b, next)))
    ???
  }

  def appendViaFoldRight[A](l: List[A], r: List[A]): List[A] =
    foldRight(l, r)((next, acc) => Cons(next, acc))

  def appendViaFoldLeft[A](l: List[A], r: List[A]): List[A] =
    foldLeft(reverse(l), r)((acc, next) => Cons(next, acc))

  def concat[A](ll: List[List[A]]): List[A] =
    foldLeft(reverse(ll), Nil: List[A])(
      (acc, next) => appendViaFoldLeft(next, acc)
    )

  def increment(l: List[Int]): List[Int] =
    foldRight(l, Nil: List[Int])((next, acc) => Cons(next + 1, acc))

  def map[A, B](l: List[A])(f: A => B): List[B] =
    foldRight(l, Nil: List[B])((next, acc) => Cons(f(next), acc))

  def filter[A](l: List[A])(f: A => Boolean): List[A] =
    foldRight(l, Nil: List[A])(
      (next, acc) => if (f(next)) Cons(next, acc) else acc
    )

  def flatMap[A, B](l: List[A])(f: A => List[B]): List[B] = concat(map(l)(f))

  def filterViaFlatMap[A](l: List[A])(f: A => Boolean): List[A] =
    flatMap(l)(a => if (f(a)) List(a) else Nil)

  def zipWith[A, B, C](as: List[A], bs: List[B])(f: (A, B) => C): List[C] =
    (as, bs) match {
      case (Cons(a, as), Cons(b, bs)) => Cons(f(a, b), zipWith(as, bs)(f))
      case _                          => Nil
    }

  @annotation.tailrec
  def hasSubsequence[A](sup: List[A], sub: List[A]): Boolean = {
    def startsWith(xs: List[A], ys: List[A]): Boolean = {
      (xs, ys) match {
        case (Cons(x, xs), Cons(y, ys)) =>
          if (x == y) startsWith(xs, ys) else false
        case (Nil, Nil) => true
        case _          => false
      }
    }

    sup match {
      case Nil                                => false
      case xs: Cons[A] if startsWith(xs, sub) => true
      case _                                  => hasSubsequence(tail(sup), sub)
    }
  }
}
