package fpinscala.laziness

import Stream._
sealed trait Stream[+A] {

  def toList: List[A] = this match {
    case Empty      => Nil
    case Cons(h, t) => h() :: t().toList
  }

  def foldRight[B](b: => B)(f: (A, => B) => B): B = this match {
    case Empty      => b
    case Cons(h, t) => f(h(), t().foldRight(b)(f))
  }

  def exists(p: A => Boolean): Boolean =
    foldRight(false)((a, b) => p(a) || b) // Here `b` is the unevaluated recursive step that folds the tail of the stream. If `p(a)` returns `true`, `b` will never be evaluated and the computation terminates early.

  @annotation.tailrec
  final def find(f: A => Boolean): Option[A] = this match {
    case Empty      => None
    case Cons(h, t) => if (f(h())) Some(h()) else t().find(f)
  }

  def take(n: Int): Stream[A] = this match {
    case Cons(h, t) if n > 1  => Cons(h, () => t().take(n - 1))
    case Cons(h, _) if n == 1 => Cons(h, () => empty)
    case _                    => Empty
  }

  def drop(n: Int): Stream[A] = this match {
    case Cons(_, t) if n > 0 => t().drop(n - 1)
    case _                   => this
  }

  def takeWhile(p: A => Boolean): Stream[A] = this match {
    case Cons(h, t) if p(h()) => Cons(h, () => t().takeWhile(p))
    case _                    => empty
  }

  def forAll(p: A => Boolean): Boolean =
    foldRight(true)((next, acc) => p(next) && acc)

  def takeWhileViaFoldRight(p: A => Boolean): Stream[A] =
    foldRight(empty[A])((a, b) => if (p(a)) cons(a, b) else empty)

  def headOption: Option[A] =
    foldRight(None: Option[A])((a, b) => Some(a).orElse(b))

  // 5.7 map, filter, append, flatmap using foldRight. Part of the exercise is
  // writing your own function signatures.

  def map[B](f: A => B): Stream[B] =
    foldRight(empty[B])((a, b) => cons(f(a), b))

  def filter(f: A => Boolean): Stream[A] =
    foldRight(empty[A])((a, b) => if (f(a)) cons(a, b) else b)

  def append[B >: A](other: => Stream[B]): Stream[B] =
    foldRight(other)((a, b) => cons(a, b))

  def flatMap[B](f: A => Stream[B]): Stream[B] =
    foldRight(empty[B])((a, b) => f(a).append(b))

  def mapViaUnfold[B](f: A => B): Stream[B] = unfold(this) {
    case Cons(h, t) => Some((f(h()), t()))
    case Empty      => None
  }

  def takeViaUnfold(n: Int): Stream[A] = unfold((this, n)) {
    case (Cons(h, t), n) if n > 0 => Some((h(), (t(), n - 1)))
    case _                        => None
  }

  def takeWhileViaUnfold(f: A => Boolean): Stream[A] = unfold(this) {
    case Cons(h, t) if f(h()) => Some((h(), t()))
    case _                    => None
  }

  def zipWith[B, C](b: Stream[B])(f: (A, B) => C): Stream[C] =
    unfold((this, b)) {
      case (Cons(h1, t1), Cons(h2, t2)) => Some((f(h1(), h2()), (t1(), t2())))
      case _                            => None
    }

  def zipAll[B](b: Stream[B]): Stream[(Option[A], Option[B])] =
    unfold((this, b)) {
      case (Cons(h1, t1), Cons(h2, t2)) =>
        val a = (Some(h1()), Some(h2()))
        val s = (t1(), t2())
        Some((a, s))
      case (Cons(h1, t1), Empty) =>
        val a = (Some(h1()), None)
        val s = (t1(), Empty)
        Some((a, s))
      case (Empty, Cons(h2, t2)) =>
        val a = (None, Some(h2()))
        val s = (Empty, t2())
        Some((a, s))
      case (Empty, Empty) => None
    }

  def startsWith[B](s: Stream[B]): Boolean = zipWith(s)(_ != _).exists(identity)

  def tails: Stream[Stream[A]] = unfold(this) {
    case s @ Cons(_, t) => Some((s, t()))
    case _              => None
  }

  def hasSubsequence[B >: A](s: Stream[B]): Boolean =
    tails exists (_.startsWith(s))

  def scanRight[B](b: B)(f: (A, => B) => B): Stream[B] =
    foldRight((b, Stream(b))) { (next, acc) =>
      lazy val acc2 = acc
      val nextB = f(next, acc2._1)
      val nextS = cons(nextB, acc2._2)
      (nextB, nextS)
    }._2
}
case object Empty extends Stream[Nothing]
case class Cons[+A](h: () => A, t: () => Stream[A]) extends Stream[A]

object Stream {
  def cons[A](hd: => A, tl: => Stream[A]): Stream[A] = {
    lazy val head = hd
    lazy val tail = tl
    Cons(() => head, () => tail)
  }

  def empty[A]: Stream[A] = Empty

  def apply[A](as: A*): Stream[A] =
    if (as.isEmpty) empty
    else cons(as.head, apply(as.tail: _*))

  val ones: Stream[Int] = Stream.cons(1, ones)

  def constant[A](a: A): Stream[A] = Stream.cons(a, constant(a))

  def from(n: Int): Stream[Int] = Stream.cons(n, from(n + 1))

  val fib: Stream[Int] = {
    def go(a: Int, b: Int): Stream[Int] = Stream.cons(b, go(a + b, a))
    go(1, 0)
  }

  def unfold[A, S](z: S)(f: S => Option[(A, S)]): Stream[A] =
    f(z) match {
      case Some((a, zz)) => Stream.cons(a, unfold(zz)(f))
      case None          => Stream.empty
    }

  val onesViaUnfold: Stream[Int] = unfold(1)(z => Some((z, z)))

  def constantViaUnfold[A](a: A): Stream[A] = unfold(a)(z => Some((z, z)))

  def fromViaUnfold(n: Int): Stream[Int] = unfold(n)(s => Some(s, s + 1))

  val fibViaUnfold: Stream[Int] = unfold((0, 1)) {
    case (b, a) => Some((b, (a, a + b)))
  }
}
