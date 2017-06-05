package iomonad13

import monads11.Monad

trait IOMonad[F[_]] extends Monad[F] {
  def forever[A, B](a: F[A]): F[B] = {
    lazy val t: F[B] = forever(a)
    flatMap(a)(_ => t)
  }
}