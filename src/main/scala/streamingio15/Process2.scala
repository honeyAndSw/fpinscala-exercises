package streamingio15.process02

trait Process[F[_], O] {

  import streamingio15.process02.Process._

  /**
    * this가 정상적인 경우에 Process p를 마지막에 추가
    */
  def ++(p: => Process[F, O]): Process[F, O] = this.onHalt {
    case End => p // 정상 종료일 때만 p를 참조, 즉 둘째 process로 나아감
    case err => Halt(err) // 정상 종료가 아니면 오류를 유지
  }

  /**
    * this에 항상 p를 추가
    * p를 this process의 `정리 동작`으로 생각할 수도 있다.
    */
  def onComplete(p: => Process[F, O]): Process[F, O] = this.onHalt {
    case End => p.asFinalizer
    case err => p.asFinalizer ++ Halt(err) // 정상 종료가 아니어도 p를 실행하고, 오류를 유지
  }

  def asFinalizer: Process[F, O] = this match {
    case Emit(h, t) => Emit(h, t.asFinalizer)
    case Halt(e) => Halt(e)
    case Await(req, recv) => await(req) {
      case Left(kill) => this.asFinalizer
      case x => recv(x)
    }
  }

  def flatMap[O2](f: O => Process[F, O2]): Process[F, O2] = this match {
    case Halt(err) => Halt(err)
    case Emit(h, t) => Try(f(h)) ++ t.flatMap(f)
    case Await(req, recv) => Await(req, recv andThen (_.flatMap(f)))
  }

  /**
    * Throwable이 발생했을 때 Process로 감싸주는
    * helper function을 사용한다.
    */
  def onHalt(f: Throwable => Process[F, O]): Process[F, O] = this match {
    case Halt(e) => Try(f(e))
    case Emit(h, t) => Emit(h, t.onHalt(f))
    case Await(req, recv) => Await(req, recv andThen (_.onHalt(f)))
  }
}

object Process {

  /* 오류를 처리할 수 있는 Await */
  case class Await[F[_], A, O](
    req: F[A],
    recv: Either[Throwable, A] => Process[F, O])
  extends Process[F, O]

  case class Emit[F[_], O](head: O, tail: Process[F, O]) extends Process[F, O]

  case class Halt[F[_], O](err: Throwable) extends Process[F, O]

  /* 정상 종료를 뜻하는 Exception */
  case object End extends Exception
  /* 강제 종료를 뜻하는 Exception */
  case object Kill extends Exception

  /**
    * Await 생성자를 커링한 helper
    */
  def await[F[_], A, O](req: F[A])(recv: Either[Throwable, A] => Process[F, O]): Process[F, O] =
    Await(req, recv)

  def Try[F[_], O](p: => Process[F, O]): Process[F, O] =
    try p
    catch { case e: Throwable => Halt(e)}

  case class Is[I]() {
    sealed trait f[X]
    val Get = new f[I] {}
  }

  type Process1[I,O] = Process[Is[I]#f, O]

  case class T[I, I2]() {
    sealed trait f[X] {
      def get: Either[I => X, I2 => X]
    }

    val L = new f[I] {
      override def get = Left(identity)
    }

    val R = new f[I2] {
      override def get = Right(identity)
    }
  }

  type Tee[I, I2, O] = Process[T[I, I2]#f, O]
}
