package fpinscala.errorhandling

import scala.{
  Option => _,
  Some => _,
  Either => _,
  _
} // hide std library `Option`, `Some` and `Either`, since we are writing our own in this chapter

sealed trait Option[+A] {
  def map[B](f: A => B): Option[B] = this match {
    case Some(get) => Some(f(get))
    case None      => None
  }

  def getOrElse[B >: A](default: => B): B = this match {
    case Some(get) => get
    case None      => default
  }

  def flatMap[B](f: A => Option[B]): Option[B] = this match {
    case Some(get) => f(get)
    case None      => None
  }

  def orElse[B >: A](ob: => Option[B]): Option[B] = this match {
    case s: Some[_] => s
    case None       => ob
  }

  def filter(f: A => Boolean): Option[A] = this match {
    case Some(get) if f(get) => Some(get)
    case _                   => None
  }
}
case class Some[+A](get: A) extends Option[A]
case object None extends Option[Nothing]

object Option {
  def failingFn(i: Int): Int = {
    val y: Int = throw new Exception("fail!") // `val y: Int = ...` declares `y` as having type `Int`, and sets it equal to the right hand side of the `=`.
    try {
      val x = 42 + 5
      x + y
    } catch { case e: Exception => 43 } // A `catch` block is just a pattern matching block like the ones we've seen. `case e: Exception` is a pattern that matches any `Exception`, and it binds this value to the identifier `e`. The match returns the value 43.
  }

  def failingFn2(i: Int): Int = {
    try {
      val x = 42 + 5
      x + ((throw new Exception("fail!")): Int) // A thrown Exception can be given any type; here we're annotating it with the type `Int`
    } catch { case _: Exception => 43 }
  }

  def mean(xs: Seq[Double]): Option[Double] =
    if (xs.isEmpty) None
    else Some(xs.sum / xs.length)

  def variance(xs: Seq[Double]): Option[Double] =
    mean(xs).flatMap { m =>
      mean(xs.map(x => math.pow(x - m, 2)))
    }

  def map2[A, B, C](a: Option[A], b: Option[B])(f: (A, B) => C): Option[C] =
    for {
      a <- a
      b <- b
    } yield f(a, b)

  def sequence[A](as: List[Option[A]]): Option[List[A]] =
    as.foldRight[Option[List[A]]](Some(Nil))(
      (next, acc) => map2(next, acc)(_ :: _)
    )

  def traverse[A, B](as: List[A])(f: A => Option[B]): Option[List[B]] =
    as.foldRight[Option[List[B]]](Some(Nil))(
      (next, acc) => map2(f(next), acc)(_ :: _)
    )

  def sequenceViaTraverse[A](as: List[Option[A]]): Option[List[A]] =
    traverse(as)(identity)
}
