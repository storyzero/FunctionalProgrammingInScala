package ch10

/**
  * Created by seed on 2016. 11. 30..
  */
trait Monoid[A] {
  def op(a1: A, a2: A): A
  def zero: A
}
