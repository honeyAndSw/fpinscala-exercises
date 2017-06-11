package iomonad13

import iomonad13.{IOMonad => Monad}
import parallelism07.Par
import parallelism07.Par._

/*

1. 효과의 추출
  부수효과가 있는 함수 안에서 순수 함수를 추출 => 서술 & 해석기

2. 간단한 입출력 형식
  trait IO { def run: Unit }
  값을 산출하는 계산을 표현할 수 없으므로,
  trait IO[A] { def run: A ... } + map, flatMap ==> for-compression, Monad[IO]
  <문제점> 1) StackOverflowError
         2) IO[A] 형식의 값이 불투명함, 너무 일반적임
         3) 동시성이나 비동기적 연산에 대해 아무것도 알지 못함

3. StackOverflowError 방지 - <문제점 1>
  def flatMap = new IO[B] { def run = f(self.run).run }
  원하는 제어 흐름을 자료 형식에 명시 ==> 생성자와 해석기(꼬리재귀 사용 가능)
  일종의 coroutine 이며 트램펄린 적용 기법
  IO는 꼬리 호출 제거를 위한 모나드, IO ==> TailRec

4. 좀 더 정교한 IO 형식 - <문제점 2>
  4.1
  유보된 함수가 비동기 실행을 지원하도록,
  Suspend[A](resume: () => A) ==> Suspend[A](resume: Par[A])
  function0이나 Par를 추상화해서 trait Free[F[_], A] ==> 자유모나드
  4.2
  단, flatMap을 구현할 수 없는 trait는 run을 사용 못함
  trait Translate[F[_],G[_]]
  def runFree(free: Free[F,A])(t: F ~> G)(implicit G: Monad[G])

5. 비차단 비동기 입출력
6. 범용 IO 형식
7. IO 형식이 스트림 방식 입출력에 충분하지 않은 이유

 */

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
