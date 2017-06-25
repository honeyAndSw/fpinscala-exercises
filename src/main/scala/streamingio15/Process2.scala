package streamingio15.process02

trait Process[F[_], O]

object Process {

  case class Await[F[_], A, O](
    req: F[A],
    recv: Either[Throwable, A] => Process[F, O])
  extends Process[F, O]

  case class Emit[F[_], O](head: O, tail: Process[F, O]) extends Process[F, O]

  case class Halt[F[_], O](err: Throwable) extends Process[F, O]


}