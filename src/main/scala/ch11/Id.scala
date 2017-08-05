package ch11

/**
  * Created by seed on 2016. 12. 8..
  */
case class Id[A](value: A) {
  def map[B](f: A => B): Id[B] = Id(f(value))

  def flatMap[B](f: A => Id[B]): Id[B] = f(value)

  val idMonad = new Monad[Id] {
    def unit[A](a: => A) = Id(a)
    override def flatMap[A,B](ida: Id[A])(f: A => Id[B]): Id[B] = ida flatMap f
  }
}