package ch11

import ch6.State
import ch7.Par
import ch7.Par.Par
import ch9.Parsers


/**
  * Created by seed on 2016. 12. 7..
  */
trait Monad[F[_]] extends Funtor[F] {
  def unit[A](a: => A): F[A]

  //def ccompose[A,B,C](f: A => F[B], g: B => F[C]): A => F[C]
  def flatMap[B,C](ma: F[B])(g: B => F[C]): F[C] = ???

  override def map [A,B](ma: F[A])(f: A => B): F[B] =
    flatMap(ma)(a => unit(f(a)))

  def map2[A,B,C](ma: F[A], mb: F[B])(f: (A,B) => C): F[C] =
    flatMap(ma)(a => map(mb)(b => f(a,b)))

  // 11.3
  // lma -> mla
  def sequence[A](lma: List[F[A]]): F[List[A]] =
    lma.foldRight(unit(List[A]()))((ma, mla) => map2(ma, mla)(_ :: _))

  // la -> mlb
  // f: a -> b
  def traverse[A,B](la: List[A])(f: A => F[B]): F[List[B]] =
    la.foldRight(unit(List[B]()))((a, mlb) => map2(f(a), mlb)(_ :: _))

  // 11.4
  def replicateM[A](n: Int, ma: F[A]): F[List[A]] =
    sequence(List.fill(n)(ma))

  // 11.6
  def filterM[A](ms: List[A])(f: A => F[Boolean]): F[List[A]] =
    ms match {
      case Nil => unit(Nil)
      case h :: t =>
        val fb = f(h)
        flatMap(fb)(b =>
          if (!b) filterM(t)(f)
          else map(filterM(t)(f))(h :: _))
    }

  // 11.7
  def compose[A,B,C](f: A => F[B], g: B => F[C]): A => F[C] =
    a => flatMap(f(a))(g)

  // 11.9
  // flatMap(flatMap(x)(f))(g) == flatMap(x)(a => flatMap(f(a))(g))
  // compose(compose(f, g), h) == compose(f, compose(g, h))

  // 11.12
  def join[A](mma: F[F[A]]): F[A] =
    flatMap(mma)(ma => ma)

  // 11.13
  def joinFlatMap[A,B](ma: F[A])(f: A => F[B]): F[B] =
    join(map(ma)(f))

  def joinCompose[A,B,C](f: A => F[B], g: B => F[C]): A => F[C] =
    a => join(map(f(a))(g))





  // 11.3
  // lma > mla
  def ssequence[A](lma: List[F[A]]): F[List[A]] =
    lma.foldRight(unit(List[A]()))((ma, mla) => map2(ma, mla)(_ :: _))

  def ttraverse[A,B](la: List[A])(f: A => F[B]): F[List[B]] =
    la.foldRight(unit(List[B]()))((a, mla) => map2(f(a), mla)(_ :: _))

  // 11.4
  def rreplicateM[A](n: Int, ma: F[A]): F[List[A]] =
    sequence(List.fill(n)(ma))

  // 11.6
  // ms => f:true => fms
  def ffilterM[A](ms: List[A])(f: A => F[Boolean]): F[List[A]] =
  ms.foldRight(unit(List[A]()))((a, fb) => map2(f(a), fb)((abool, bs) => if (abool) a :: bs else bs))
//  ms match {
//    case Nil => unit(Nil)
//    case h :: t =>
//      flatMap(f(h))(b => if (b) map(ffilterM(t)(f))(h :: _) else ffilterM(t)(f))
//  }

  // 11.7
  def ccompose[A,B,C](f: A => F[B], g: B => F[C]): A => F[C] =
    a => {
      flatMap(f(a))(b => g(b))
    }
}

object Monad {
  // 11.1
  val pparMonad = new Monad[Par] {
    def unit[A](a: => A): Par[A] = Par.unit(a)

    override def flatMap[A, B](ma: Par[A])(f: (A) => Par[B]): Par[B] =
      Par.flatMap(ma)(f)
  }

  def pparserMonad[P[+_]](p: Parsers[P]) = new Monad[P] {
    def unit[A](a: => A): P[A] = p.succeed(a)

    override def flatMap[A, B](ma: P[A])(f: (A) => P[B]): P[B] =
      p.flatMap(ma)(f)
  }

  val ooptionMonad = new Monad[Option] {
    def unit[A](a: => A): Option[A] = Some(a)

    override def flatMap[A, B](ma: Option[A])(f: (A) => Option[B]): Option[B] =
      ma.flatMap(f)
  }

  val sstreamMonad = new Monad[Stream] {
    def unit[A](a: => A): Stream[A] = Stream(a)

    override def flatMap[A, B](ma: Stream[A])(f: (A) => Stream[B]): Stream[B] =
      ma.flatMap(f)
  }

  val llistMonad = new Monad[List] {
    def unit[A](a: => A): List[A] = List(a)

    override def flatMap[A, B](ma: List[A])(f: (A) => List[B]): List[B] =
      ma.flatMap(f)
  }

  // 11.2
  // 정떨어짐 by clint.cho






















  // 11.1
  val parMonad = new Monad[Par] {
    def unit[A](a: => A) = Par.unit(a)

    override def flatMap[A,B](ma: Par[A])(f: A => Par[B]) = Par.flatMap(ma)(f)
  }

  def parserMonad[P[+_]](p: Parsers[P]) = new Monad[P] {
    def unit[A](a: => A) = p.succeed(a)

    override def flatMap[A,B](ma: P[A])(f: A => P[B]) = p.flatMap(ma)(f)
  }

  val optionMonad = new Monad[Option] {
    def unit[A](a: => A) = Some(a)

    override def flatMap[A, B](ma: Option[A])(f: (A) => Option[B]) = ma.flatMap(f)
  }

  val streamMonad = new Monad[Stream] {
    def unit[A](a: => A) = Stream(a)

    override def flatMap[A, B](ma: Stream[A])(f: (A) => Stream[B]) = ma.flatMap(f)
  }

  val listMonad = new Monad[List] {
    def unit[A](a: => A) = List(a)

    override def flatMap[A, B](ma: List[A])(f: (A) => List[B]) = ma.flatMap(f)
  }

  // 11.2
  class StateMonads[S] {
    type StateS[A] = State[S, A]

    val monad = new Monad[StateS] {
      def unit[A](a: => A): State[S, A] = State(s => (a, s))

      override def flatMap[A,B](st: State[S, A])(f: A => State[S, B]): State[S, B] = st.flatMap(f)
    }
  }

  def stateMonad[S] = new Monad[({type lambda[x] = State[S, x]})#lambda] {
    def unit[A](a: => A): State[S, A] = State(s => (a, s))

    override def flatMap[A,B](st: State[S, A])(f: A => State[S, B]): State[S, B] = st flatMap f
  }

}