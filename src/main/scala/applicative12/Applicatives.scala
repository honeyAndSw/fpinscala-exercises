package applicative12

import monads11.{Functor, Monad}

trait Applicative[F[_]] extends Functor[F] {
  // Applicative의 기본수단
  def map2[A,B,C](fa: F[A], fb: F[B])(f: (A, B) => C): F[C]
  // 연습문제 12.2
  //  { val a1: F[B => C] = apply[A, B => C](unit(f.curried))(fa)
  //    apply(a1)(fb) }

  def unit[A](a: => A): F[A]

  def map[A,B](fa: F[A])(f: A => B): F[B] = map2(fa, unit(()))((a, _) => f(a))
  // 연습문제 12.2
  // apply(unit(f))(fa)

  /**
    * 연습문제 12.2
    * Applicative의 또 다른 기본수단인 unit과 apply
    * unit과 map2로도 구현할 수 있다.
    */
  def apply[A,B](fab: F[A => B])(fa: F[A]): F[B] = map2(fab, fa)((f: A => B, a: A) => f(a))

  def traverse[A,B](as: List[A])(f: A => F[B]): F[List[B]] =
    as.foldRight(unit(List[B]()))((a, acc) => map2(f(a), acc)(_ :: _))

  /**
    * 연습문제 12.1
    */
  def sequence[A](lma: List[F[A]]): F[List[A]] = traverse(lma)(fa => fa)
  def replicateM[A](n: Int, fa: F[A]): F[List[A]] = sequence(List.fill(n)(fa)) //(for (i <- 0 until n) yield fa).toList
  def product[A,B](fa: F[A], fb: F[B]): F[(A, B)] = map2(fa, fb)((_, _))
}

object Applicative {
  /**
    * 연습문제 12.5
    */
  def eitherMonad[E] = new Monad[({type f[x] = Either[E, x]})#f] {
    override def unit[A](a: => A): Either[E, A] = Right(a)
    override def flatMap[A, B](fa: Either[E, A])(f: (A) => Either[E, B]): Either[E, B] = fa flatMap f
  }
}
