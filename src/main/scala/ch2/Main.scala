package ch2

/**
  * Created by seed on 2017. 1. 31..
  */
class Main {
  // 2.3
  def curry[A,B,C](f: (A,B) => C): A => (B => C) =
    (a) => (b) => f(a, b)

  // 2.4
  def uncurry[A,B,C](f: A => B => C): (A, B) => C =
    (a, b) => f(a)(b)

  // 2.6
  def compose[A,B,C](f: B => C, g: A => B): A => C =
    (a) => f(g(a))
}
