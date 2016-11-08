package ch8

import ch8.Prop.{FailedCase, SuccessCount}
import org.scalacheck.Gen
import org.scalacheck.Prop.forAll

/**
  * Created by seed on 2016. 11. 7..
  */
object Main extends App {
  val intList = Gen.listOf(Gen.choose(0, 100))

  val prop =
    forAll(intList)(ns => ns.reverse.reverse == ns) &&
    forAll(intList)(ns => ns.headOption == ns.reverse.lastOption)

  val failingProp =
    forAll(intList)(ns => ns.reverse == ns)

  // prop.check
  // failingProp.check

  // exercises 8.3
}

class Gen[A] {
  def listOf[A](a: Gen[A]): Gen[List[A]] = ???
  def listOfN[A](n: Int, a: Gen[A]): Gen[List[A]] = ???
  def forAll[A](a: Gen[A])(f: A => Boolean): Prop
}

trait Prop {
  def check: Either[(FailedCase, SuccessCount), SuccessCount] = ???
  def &&(p: Prop): Prop = ???


}

object Prop {
  type FailedCase = String
  type SuccessCount = Int
}