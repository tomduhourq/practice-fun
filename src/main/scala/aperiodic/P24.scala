package aperiodic

import scala.util.{Random, Try}

object P24 extends App {
  def lotto(howMany: Int, upTo: Int): List[Int] = {
    require(upTo >= 1, "upTo parameter must be greater or equal to 1")
    Stream continually(Random.nextInt(upTo) + 1) take howMany toList
  }

  println(lotto(6, 49))
  Try { lotto(-1, 10) } recover { case _: IllegalArgumentException => println("Success!") }
}
