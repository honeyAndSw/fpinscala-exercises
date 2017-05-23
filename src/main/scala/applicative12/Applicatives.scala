package applicative12

import monads11.{Functor, Monad}
import monoids10.Monoid

trait Applicative[F[_]] extends Functor[F] {
  /////////////// Applicative의 기본수단 ///////////////
  def map2[A,B,C](fa: F[A], fb: F[B])(f: (A, B) => C): F[C]
  def unit[A](a: => A): F[A]

  /**
    * 연습문제 12.2
    */
  def map2WithApply[A,B,C](fa: F[A], fb: F[B])(f: (A, B) => C): F[C] = {
    val a1: F[B => C] = apply[A, B => C](unit(f.curried))(fa)
    apply(a1)(fb)
  }

  /**
    * 연습문제 12.?
    */
  def map2WithProduct[A,B,C](fa: F[A], fb: F[B])(f: (A, B) => C): F[C] = map[(A,B),C](product(fa, fb))(f.tupled)

  def map[A,B](fa: F[A])(f: A => B): F[B] = map2(fa, unit(()))((a, _) => f(a))

  /**
    * 연습문제 12.2
    */
  def mapWithApply[A,B](fa: F[A])(f: A => B): F[B] = apply[A,B](unit(f))(fa)

  /**
    * 연습문제 12.2
    * Applicative의 또 다른 기본수단인 unit과 apply
    * unit과 map2로도 구현할 수 있다.
    */
  def apply[A,B](fab: F[A => B])(fa: F[A]): F[B] = map2(fab, fa)((f: A => B, a: A) => f(a))

  /**
    * 연습문제 12.?
    * Applicative의 또 다른 기본수단인 product, map, unit
    */
  def product[A,B](fa: F[A], fb: F[B]): F[(A, B)] = map2(fa, fb)((_, _))

  def traverse[A,B](as: List[A])(f: A => F[B]): F[List[B]] =
    as.foldRight(unit(List[B]()))((a, acc) => map2(f(a), acc)(_ :: _))

  /**
    * 연습문제 12.1
    */
  def sequence[A](lma: List[F[A]]): F[List[A]] = traverse(lma)(fa => fa)
  def replicateM[A](n: Int, fa: F[A]): F[List[A]] = sequence(List.fill(n)(fa)) //(for (i <- 0 until n) yield fa).toList
  def product[A,B](fa: F[A], fb: F[B]): F[(A, B)] = map2(fa, fb)((_, _))

  /**
    * 연습문제 12.12
    */
  def sequenceMap[K,V](m: Map[K, F[V]]): F[Map[K, V]] =
    m.foldRight(unit(Map.empty[K,V])){ case ((k: K, fv: F[V]), fm) =>
      map2(fv, fm)((v: V, m: Map[K,V]) => m + ((k, v))) // (k -> v)
    }

  /**
    * 연습문제 12.3
    */
  def map3[A,B,C,D](fa: F[A], fb: F[B], fc: F[C])(f: (A, B, C) => D): F[D] =
    apply(
      apply(
        apply[A, B => C => D](unit(f.curried))(fa)
      )(fb)
    )(fc)

  def map4[A,B,C,D,E](fa: F[A], fb: F[B], fc: F[C], fd: F[D])(f: (A, B, C, D) => E): F[E] =
    apply(
      apply(
        apply(
          apply[A, B => C => D => E](unit(f.curried))(fa)
        )(fb)
      )(fc)
    )(fd)
}

object Applicative {
  /**
    * 연습문제 12.5
    */
  def eitherMonad[E] = new Monad[({type f[x] = Either[E, x]})#f] {
    override def unit[A](a: => A): Either[E, A] = Right(a)
    override def flatMap[A, B](fa: Either[E, A])(f: (A) => Either[E, B]): Either[E, B] = fa flatMap f
  }

  sealed trait Validation[+E, +A]
  case class Failure[E](head: E, tail: Vector[E] = Vector()) extends Validation[E, Nothing]
  case class Success[A](a: A) extends Validation[Nothing, A]

  type Const[M, B] = M
  implicit def monoidApplicative[M](M: Monoid[M]) = new Applicative[({ type f[x] = Const[M,x] })#f] {
    override def map2[A, B, C](m1: M, m2: M)(f: (A, B) => C): M = M.op(m1, m2)
    override def unit[A](a: => A): M = M.zero
  }
}

/********** Traverse **********/

trait Traverse[F[_]] extends Functor[F] {
  def traverse[G[_] : Applicative, A, B](fa: F[A])(f: A => G[B])(implicit G: Applicative[G]): G[F[B]] = sequence(map(fa)(f))
  def sequence[G[_] : Applicative, A](fba: F[G[A]])(implicit G: Applicative[G]): G[F[A]] = traverse(fba)(ma => ma)
}