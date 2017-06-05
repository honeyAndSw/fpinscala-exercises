package iomonad13

import iomonad13.{IOMonad => Monad}

object IOTailRec {
  sealed trait TailRec[A] {
    def flatMap[B](f: A => TailRec[B]): TailRec[B] = FlatMap(this, f)
    def map[B](f: A => B): TailRec[B] = flatMap(f.andThen(Return(_)))
  }

  case class Return[A](a: A) extends TailRec[A]
  case class Suspend[A](resume: () => A) extends TailRec[A]
  case class FlatMap[A,B](sub: TailRec[A], k: A => TailRec[B]) extends TailRec[B]

  /////////////// Do PrintLine Forever. ///////////////
  def printLine(s: String): TailRec[Unit] = Suspend(() => println(s))

  object TailRec extends Monad[TailRec] {
    override def unit[A](a: => A): TailRec[A] = Return(a)
    override def flatMap[A, B](fa: TailRec[A])(f: (A) => TailRec[B]): TailRec[B] = fa.flatMap(f)
    def apply[A](a: => A): TailRec[A] = unit(a) // syntax for IO { .. }
  }

  @annotation.tailrec
  def run[A](io: TailRec[A]): A = io match {
    case Return(a) => a
    case Suspend(r) => r()
    case FlatMap(x, f) => x match {
      case Return(a1) => run(f(a1))
      case Suspend(r) => run(f(r()))
      case FlatMap(y, g) => run(y flatMap (a => g(a) flatMap f))
    }
  }
}