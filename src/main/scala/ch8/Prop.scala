package ch8

import ch8.Prop.{FailedCase, SuccessCount}

/**
  * Created by seed on 2016. 11. 9..
  */
trait Prop {
  def check: Either[(FailedCase, SuccessCount), SuccessCount] = ???
  def &&(p: Prop): Prop = ???
}

object Prop {
  type FailedCase = String
  type SuccessCount = Int
}

// 8.3
//trait Prop {
//  def check: Boolean
//  def &&(p: Prop): Prop = new Prop {
//    def check = Prop.this.check && p.check
//  }
//}