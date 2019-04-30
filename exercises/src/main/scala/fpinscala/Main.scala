package fpinscala

import fpinscala.laziness._
import fpinscala.laziness.Stream._

object Main {
  def main(args: Array[String]): Unit = {
    println(Stream(1, 2, 3).scanRight(0)(_ + _).toList)
  }
}
