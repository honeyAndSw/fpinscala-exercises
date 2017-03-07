package laziness05

sealed trait Stream[+A] {
  def headOption: Option[A] = this match {
    case Empty => None
    case Cons(h, t) => Some(h())
  }

  /**
    * 연습문제 5.1
    * Stream의 평가를 강제한 List로
    */
  def toList: List[A] = this match {
    case Empty => Nil
    case Cons(h, t) => h() +: t().toList
  }

  /**
    * 연습문제 5.2
    * 처음 n개의 요소를 돌려주는 함수
    */
  def take(n: Int): Stream[A] = this match {
    case Cons(h, t) if (n > 0) => Stream.cons[A](h(), t().take(n - 1))
    case _ => Empty
  }

  /**
    * 연습문제 5.2
    * 처음 n개의 요소를 건너뛴 Stream을 돌려주는 함수
    */
  def drop(n: Int): Stream[A] = ???
}

case object Empty extends Stream[Nothing]
case class Cons[+A](h: () => A, t: () => Stream[A]) extends Stream[A]

object Stream {
  def cons[A](hd: => A, tl: => Stream[A]): Stream[A] = {
    lazy val head = hd
    lazy val tail = tl
    Cons(() => head, () => tail)
  }

  def empty[A]: Stream[A] = Empty

  def apply[A](as: A*): Stream[A] = if (as.isEmpty) empty else cons(as.head, apply(as.tail: _*))
}