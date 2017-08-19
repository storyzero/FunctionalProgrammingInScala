package ch3

/**
  * Created by seed on 2017. 1. 31..
  */

sealed trait List[+A]
case object Nil extends List[Nothing]
case class Cons[+A](head: A, tail: List[A]) extends List[A]

object List {
  def apply[A](as: A*): List[A] = 
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))

  def sum(ints: List[Int]): Int = ints match {
    case Nil => 0
    case Cons(x, xs) => x + sum(xs)
  }

  def product(ds: List[Double]): Double = ds match {
    case Nil => 0
    case Cons(0.0, _) => 0.0
    case Cons(x, xs) => x * product(xs)
  }

  // 3.1
  // List matches the third pattern therefore answer is 3
  val x = List(1,2,3,4,5) match {
    case Cons(x, Cons(2, Cons(4, _))) => x
    case Nil => 42
    case Cons(x , Cons(y, Cons(3, Cons(4, _)))) => x + y
    case Cons(h, t) => h + sum(t)
    case _ => 101
  }

  // 3.2
  def tail[A](xs: List[A]): List[A] = xs match {
    case Nil => throw new IllegalArgumentException
    case Cons(_, t) => t
  }
  
  // 3.3
  def setHead[A](xs: List[A], x: A) = xs match {
    case Nil => throw new IllegalArgumentException
    case Cons(_, t) => Cons(x, t)
  }
}

object Main extends App {
  // 3.2 test
  // Nil
  println(List.tail(Cons(1, Nil)))
  // IAE
  // println(List.tail(Nil))
  // MPE
  // println(List.tail(null))

  // 3.3 test
  // Cons(4,Cons(2,Cons(1,Nil)))
  println(List.setHead(Cons(3,Cons(2,Cons(1,Nil))), 4))
}