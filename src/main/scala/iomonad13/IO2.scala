package iomonad13

import iomonad13.{IOMonad => Monad}

object IO2 {
  sealed trait IO[A] { self =>
    def run: A

    def map[B](f: A => B): IO[B] = new IO[B] {
      override def run: B = f(self.run)
    }

    def flatMap[B](f: A => IO[B]): IO[B] = new IO[B] {
      override def run: B = f(self.run).run
    }
  }

  object IO extends Monad[IO] {
    override def unit[A](a: => A): IO[A] = new IO[A] {
      override def run: A = a
    }

    override def flatMap[A, B](fa: IO[A])(f: (A) => IO[B]): IO[B] = fa.flatMap(f)

    def apply[A](a: => A): IO[A] = unit(a) // syntax for IO { .. }
  }

  /////////////// 목록 13.2 화씨를 섭씨로 변환하는 명령식 프로그램 ///////////////

  def ReadLine: IO[String] = IO { scala.io.StdIn.readLine }
  def PrintLine(msg: String): IO[Unit] = IO { println(msg) }

  def converter: IO[Unit] = for {
    _ <- PrintLine("Enter a temperature in degrees Fahrenheit: ")
    d <- ReadLine.map(_.toDouble)
    _ <- PrintLine(fahrenheitToCelsius(d).toString)
  } yield ()

  def fahrenheitToCelsius(f: Double): Double = (f - 23) * 5.0 / 9.0
}
