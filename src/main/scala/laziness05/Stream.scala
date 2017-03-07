package laziness05

sealed trait Stream[+A] {
  def headOption: Option[A] = this match {
    case Empty => None
    case Cons(h, t) => Some(h())
  }

  /**
    * 연습문제 5.6
    */
  def headOption2: Option[A] = foldRight[Option[A]](None)((a, acc) => Some(a))

  def exists(p: A => Boolean): Boolean = foldRight(false)((a, acc) => p(a) || acc)
//  this match {
//    case Cons(h, t) => p(h()) || t().exists(p)
//    case _ => false
//  }

  def foldRight[B](z: => B)(f: (A, => B) => B): B = this match {
    case Cons(h, t) => f(h(), t().foldRight(z)(f))
    case _ => z
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
  def drop(n: Int): Stream[A] = this match {
    case Cons(h, t) if (n > 0) => t().drop(n - 1)
    case _ => this
  }

  /**
    * 연습문제 5.3
    * 연습문제 5.5
    * 주어진 술어를 만족하는 선행 요소들을 모두 돌려주는 함수
    */
  def takeWhile(p: A => Boolean): Stream[A] =
    foldRight[Stream[A]](Stream.empty)((a, acc) => if (p(a)) Stream.apply(a) else Stream.cons(a, acc))

  /**
    * 연습문제 5.4
    */
  def forAll(p: A => Boolean): Boolean = foldRight(true)((a, acc) => p(a) && acc)
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

  /**
    * 연습문제 5.8
    */
  def constant[A](a: A): Stream[A] = cons(a, constant(a))

  /**
    * 연습문제 5.9
    */
  def from(n: Int): Stream[Int] = cons(n, from(n + 1))

  /**
    * 연습문제 5.10
    */
  def fibs(): Stream[Int] = {
    def fibs2(i1: Int, i2: Int): Stream[Int] = cons(i1, fibs2(i2, i1 + i2))
    fibs2(0, 1)
  }

  /**
    * 연습문제 5.11
    * @param z 초기상태
    * @param f 다음 상태 및 (생성된 스트림 안의) 다음 값을 산출하는 함수
    */
  def unfold[A, S](z: S)(f: S => Option[(A, S)]): Stream[A] = {
    f(z).flatMap{ as =>
      Some(cons(as._1, unfold(as._2)(f)))
    }.getOrElse(empty)
  }
}