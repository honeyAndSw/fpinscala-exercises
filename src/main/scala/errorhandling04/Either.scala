package errorhandling04

/********************************
 *          연습문제 4.6          *
 ********************************/
sealed trait Either[+E, +A] {

  def map[B](f: A => B): Either[E, B] = this match {
    case Right(a) => Right(f(a))
    case Left(e) => Left(e)
  }

  def flatMap[EE >: E, B](f: A => Either[EE, B]): Either[EE, B] = this.map(f) match {
      case Right(a) => a
      case Left(e) => Left(e)
    }

  def orElse[EE >: E, B >: A](b: => Either[EE, B]): Either[EE, B] = this match {
    case Left(e) => b
    case Right(a) => Right(a)
  }

  def map2[EE >: E, B, C](b: Either[EE, B])(f: (A, B) => C): Either[EE, C] =
    this.flatMap{ aa =>
      b.map(bb => f(aa, bb))
    }
}

case class Left[+E](value: E) extends Either[E, Nothing]
case class Right[+A](value: A) extends Either[Nothing, A]

object Either {

  def Try[A](a: => A): Either[Exception, A] =
    try Right(a)
    catch { case e: Exception => Left(e) }

  /**
    * 연습문제 4.7
    */
  def traverse[E, A, B](as: List[A])(f: A => Either[E, B]): Either[E, List[B]] = {
    as.foldRight[Either[E, List[B]]](Right(List())){
      (aa, acc) => f(aa).map2[E, List[B], List[B]](acc)(_ +: _)
    }
  }

  def sequence[E, A](es: List[Either[E, A]]): Either[E, List[A]] = traverse[E, Either[E, A], A](es)(a => a)
}


