package fpinscala

import fpinscala.state._
import fpinscala.state.State._
import fpinscala.state.RNG._

object Main {
  def main(args: Array[String]): Unit = {
    println(mapViaFlatMap(int)(i => List.fill(i)("x"))(RNG.Simple(42)))
  }
}
