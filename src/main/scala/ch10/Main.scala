package ch10

import ch8.{Gen, Prop}

/**
  * Created by seed on 2016. 11. 30..
  */
class Main {
  // 10.1
  val intAddition: Monoid[Int] = new Monoid[Int] {
    override def op(x: Int, y: Int) = x + y

    override def zero = 0
  }

  val intMultiplication: Monoid[Int] = new Monoid[Int] {
    override def op(x: Int, y: Int) = x * y

    override def zero = 1
  }

  val booleanOr: Monoid[Boolean] = new Monoid[Boolean] {
    override def op(x: Boolean, y: Boolean) = x || y

    override def zero = false
  }

  val booleanAnd: Monoid[Boolean] = new Monoid[Boolean] {
    override def op(x: Boolean, y: Boolean) = x && y

    override def zero = true
  }

  // 10.2
  def optionMonoid[A]: Monoid[Option[A]] = new Monoid[Option[A]] {
    override def op(a1: Option[A], a2: Option[A]) = a1.orElse(a2)

    override def zero = None
  }

  // 10.3
  def endoMonoid[A]: Monoid[A => A] = new Monoid[(A) => A] {
    override def op(a1: (A) => A, a2: (A) => A) = ???

    override def zero = ???
  }

  // 10.4
  def monoidLaws[A](m: Monoid[A], gen: Gen[A]): Prop = ???

  // 10.5
  def foldMap[A, B](as: List[A], m: Monoid[B])(f: A => B): B = ???

  // 10.6
  // 10.7
}