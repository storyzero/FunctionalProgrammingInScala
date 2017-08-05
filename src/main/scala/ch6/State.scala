package ch6

import ch6.State._

/**
  * Created by seed on 2016. 11. 16..
  */

case class State[S, +A](run: S => (A, S)) {

  def unit[S, A](a: A): State[S, A] =
    State(s => (a, s))

  def map[B](f: A => B): State[S, B] =
    flatMap(a => unit(f(a)))

  def map2[B,C](sb: State[S, B])(f: (A, B) => C): State[S, C] =
    flatMap(a => sb.map(b => f(a, b)))

  def flatMap[B](f: A => State[S, B]): State[S, B] = State(s => {
    val (a, s1) = run(s)
    f(a).run(s1)
  })
}

object State {
  type Rand[A] = State[RNG, A]

  def unit[S, A](a: A): State[S, A] =
    State(s => (a, s))

  def sequence[A](fs: List[Rand[A]]): Rand[List[A]] = ???
}
