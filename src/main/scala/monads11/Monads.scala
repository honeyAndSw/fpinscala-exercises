package monads11

import testing08.Gen

trait Monad[F[_]] extends Functor[F] {
  /////////////// Monad의 기본수단 ///////////////
  def unit[A](a: => A): F[A]
  def flatMap[A,B](fa: F[A])(f: A => F[B]): F[B]

  override def map[A,B](fa: F[A])(f: A => B): F[B] =
    flatMap(fa)(a => unit(f(a)))

  def map2[A,B,C](fa: F[A], fb: F[B])(f: (A, B) => C): F[C] =
    flatMap(fa)(a => map(fb)(b => f(a, b)))

  def product[A,B](fa: F[A], fb: F[B]): F[(A, B)] = map2(fa, fb)((_, _))

  // ??????????
  def codistribute2[A,B](p: (F[A], F[B])): F[(A, B)] =
    flatMap(p._1)(a => map(p._2)(b => (a, b)))

  /**
    * 연습문제 11.3
    */
  def sequence[A](lma: List[F[A]]): F[List[A]] = lma.foldLeft(unit(List[A]())) { (acc, fa) =>
    map2(acc, fa)(_ :+ _)
  }

  def traverse[A,B](la: List[A])(f: A => F[B]): F[List[B]] =
    la.foldLeft(unit(List[B]())) { (acc: F[List[B]], a: A) =>
      map2(acc, f(a))(_ :+ _)
    }
}

object Monad {
  val genMonad = new Monad[Gen] {
    override def unit[A](a: => A): Gen[A] = Gen.unit(a)
    override def flatMap[A, B](fa: Gen[A])(f: (A) => Gen[B]): Gen[B] = fa flatMap f
  }
}

trait Functor[F[_]] {
  // map(fa)(a => a) == fa
  def map[A,B](fa: F[A])(f: A => B): F[B]

  def distribute[A,B](fab: F[(A, B)]): (F[A], F[B]) =
    (map(fab)(_._1), map(fab)(_._2))

  def codistribute[A,B](e: Either[F[A], F[B]]): F[Either[A, B]] =
    e match {
      case Left(fa) => map(fa)(Left(_))
      case Right(fb) => map(fb)(Right(_))
    }
}

object Functor {
  val listFunctor = new Functor[List] {
    override def map[A, B](fa: List[A])(f: (A) => B): List[B] = fa map f
  }
}