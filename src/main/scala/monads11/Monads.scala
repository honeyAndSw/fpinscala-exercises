package monads11

import testing08.Gen

trait Monad[F[_]] extends Functor[F] {
  /////////////// Monad의 기본수단 ///////////////
  def unit[A](a: => A): F[A]
  def flatMap[A,B](fa: F[A])(f: A => F[B]): F[B]

  /**
    * 연습문제 11.7
    */
  def compose[A,B,C](f: A => F[B], g: B => F[C]): A => F[C] = a => flatMap(f(a))(g)

  /**
    * 연습문제 11.8
    * flatMap을 unit, compose로 구현 가능 ==> unit, compose가 최소 집합
    */
  def flatMap2[A,B](fa: F[A])(f: A => F[B]): F[B] = compose((_: Unit) => fa, f)(())

  /**
    * 연습문제 11.12
    */
  def join[A](mma: F[F[A]]): F[A] = flatMap(mma)(ma => ma)

  /**
    * 연습문제 11.13
    * flatMap, compose를 join과 map으로 구현 가능 ==> unit, join, map이 최소 집합
    */
  def flatMap3[A,B](fa: F[A])(f: A => F[B]): F[B] = join(map(fa)(f))
  def compose3[A,B,C](f: A => F[B], g: B => F[C]): A => F[C] = a => join(map(f(a))(g))

  override def map[A,B](fa: F[A])(f: A => B): F[B] =
    flatMap(fa)(a => unit(f(a)))

  def map2[A,B,C](fa: F[A], fb: F[B])(f: (A, B) => C): F[C] =
    flatMap(fa)(a => map(fb)(b => f(a, b)))

  def product[A,B](fa: F[A], fb: F[B]): F[(A, B)] = map2(fa, fb)((_, _))

  /**
    * 연습문제 11.3
    */
  def sequence[A](lma: List[F[A]]): F[List[A]] = traverse(lma)(fa => fa)
//  lma.foldLeft(unit(List[A]())) { (acc, fa) =>
//    map2(acc, fa)(_ :+ _)
//  }

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

  /**
    * 연습문제 11.17
    */
  case class Id[A](value: A) {
    def map[B](f: A => B): Id[B] = Id(f(value))
    def flatMap[B](f: A => Id[B]): Id[B] = f(value)
  }

  val idMonad = new Monad[Id] {
    override def unit[A](a: => A): Id[A] = Id(a)
    override def flatMap[A,B](fa: Id[A])(f: A => Id[B]): Id[B] = fa flatMap f
  }
}

trait Functor[F[_]] {
  // map(fa)(a => a) == fa
  def map[A,B](fa: F[A])(f: A => B): F[B]

  // def product[A,B](fa: F[A], fb: F[B]): F[(A, B)] = map2(fa, fb)((_, _))
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