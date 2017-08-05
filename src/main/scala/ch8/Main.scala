package ch8

import org.scalacheck.Prop.forAll

/**
  * Created by seed on 2016. 11. 7..
  */
object Main extends App {
  val intList = org.scalacheck.Gen.listOf(Gen.choose(0, 100))
  println(intList.sample)

  val prop =
    forAll(intList)(ns => ns.reverse.reverse == ns) &&
    forAll(intList)(ns => ns.headOption == ns.reverse.lastOption)

  val failingProp =
    forAll(intList)(ns => ns.reverse == ns)

   prop.check
   failingProp.check
}