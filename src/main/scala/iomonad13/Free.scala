package iomonad13

import iomonad13.{IOMonad => Monad}
import parallelism07.Par
import parallelism07.Par._

object Free {
  sealed trait Free[F[_],A] {
    def flatMap[B](f: A => Free[F,B]): Free[F,B] = FlatMap(this, f)
    def map[B](f: A => B): Free[F,B] = flatMap(f andThen (Return(_)))
  }

  case class Return[F[_],A](a: A) extends Free[F,A]
  case class Suspend[F[_],A](resume: F[A]) extends Free[F,A]
  case class FlatMap[F[_],A,B](
        sub: Free[F,A], f: A => Free[F,B]) extends Free[F,B]

  private def step[F[_],A](free: Free[F,A]): Free[F,A] = free match {
    case FlatMap(FlatMap(x, f), g) => step(x flatMap (a => f(a).flatMap(g)))
    case FlatMap(Return(x), f) => step(f(x))
    case _ => free
  }

  def runFree[F[_],G[_],A](free: Free[F,A])(t: F ~> G)(
    implicit G: Monad[G]): G[A] = step(free) match {
    case Return(a) => G.unit(a)
    case Suspend(r) => t(r)
    case FlatMap(Suspend(r), f) => G.flatMap(t(r))(a => runFree(f(a))(t))
    case _ => sys.error("Impossible; `step` eliminates these cases")
  }

  /////////////// Translate ///////////////

  trait Translate[F[_], G[_]] {
    def apply[A](f: F[A]): G[A]
  }

  type ~>[F[_], G[_]] = Translate[F, G]

  /////////////// Console ///////////////

  sealed trait Console[A] {
    def toPar: Par[A]
    def toThunk: () => A
  }

  case object ReadLine extends Console[Option[String]] {
    override def toPar: Par[Option[String]] = Par.lazyUnit(run)
    override def toThunk: () => Option[String] = () => run

    def run: Option[String] =
      try Some(readLine())
      catch {
        case e: Exception => None
      }
  }

  case class PrintLine(line: String) extends Console[Unit] {
    override def toPar: Par[Unit] = Par.lazyUnit(println(line))
    override def toThunk: () => Unit = () => println(line)
  }

  object Console {
    type ConsoleIO[A] = Free[Console, A]
    def readLn: ConsoleIO[Option[String]] = Suspend(ReadLine)
    def printLn(line: String): ConsoleIO[Unit] = Suspend(PrintLine(line))
  }
}
