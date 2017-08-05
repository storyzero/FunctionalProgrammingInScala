package ch8

import ch6.{RNG, State}

/**
  * Created by seed on 2016. 11. 9..
  */
case class Gen[+A](sample: State[RNG, A]){

//  // 8.6
//  def flatMap[B](f: A => Gen[B]): Gen[B] = {
//    f(this.sample)
//  }
//
//  // 8.6
//  def listOfN(size: Gen[Int]): Gen[List[A]] = {
//    size.flatMap(n => Gen.listOfN(n, this))
//  }

}

object Gen {

  // 8.4
  def choose(start: Int, stopExclusive: Int): Gen[Int] = {
    Gen(State(RNG.nonNegativeInt).map(n => start + n % (stopExclusive-start)))
  }

  // 8.5
  def unit[A](a: => A): Gen[A] = {
    Gen(State.unit(a))
  }

  // 8.5
  def boolean: Gen[Boolean] = {
    Gen(State(RNG.boolean))
  }

  // 8.5
  def listOfN[A](n: Int, g: Gen[A]): Gen[List[A]] = ???
//
//  // 8.7
//  def union[A](g1: Gen[A], g2: Gen[A]): Gen[A] = {
//    boolean.flatMap(b => if (b) g1 else g2)
//  }
//
//  // 8.8
//  def weighted[A](g1: (Gen[A], Double), g2: (Gen[A], Double)): Gen[A] = {
//    val total = g1._2 + g2._2
//    val first = g1._2 / total
//    val dice = scala.util.Random.nextDouble % 1
//    if (dice < first)
//      g1._1
//    else
//      g2._1
//  }

}