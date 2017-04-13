package laziness05

sealed trait Stream[+A] {

  import laziness05.Stream.{cons, empty, unfold}

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
    case Cons(h, t) if (n > 0) => cons[A](h(), t().take(n - 1))
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
    foldRight(empty[A])((h,t) => if (p(h)) cons(h,t) else empty)
    // 문제를 잘못 이해함!
    // foldRight(Stream.empty[A])((a, acc) => if (p(a)) Stream.apply(a) else Stream.cons(a, acc))

  /**
    * 연습문제 5.4
    */
  def forAll(p: A => Boolean): Boolean = foldRight(true)((a, acc) => p(a) && acc)

  /**
    * 연습문제 5.7
    * foldRight를 이용한 map, filter, append, flatMap
    */
  def map[B](p: A => B): Stream[B] = foldRight(empty[B])((a, acc) => cons(p(a), acc))

  def filter(p: A => Boolean): Stream[A] = foldRight(empty[A])((a, acc) => if (p(a)) acc else cons(a, acc))

  def append[B >: A](s: Stream[B]): Stream[B] = foldRight(s)((a, acc) => cons(a, acc))

  def flatMap[B](p: A => Stream[B]): Stream[B] = foldRight(empty[B])((a, acc) => p(a).append(acc))

  /**
    * 연습문제 5.13
    * unfold를 이용한 map, take, takeWhile, zipWith
    */
  def map2[B](p: A => B): Stream[B] = unfold[B, Stream[A]](this) { /*s => s match { 생략할 수 있다. */
    case Cons(h, t) => Some((p(h()), t()))
    case _ => None
  }

  def take2(n: Int): Stream[A] = unfold[A, (Int, Stream[A])]((n, this)){
    case (n, Cons(h, t)) if (n > 0) => Some((h(), (n - 1, t())))
    case _ => None
  }

  def takeWhile2(p: A => Boolean): Stream[A] = unfold(this) {
    case Cons(h, t) if (p(h())) => Some(h(), t())
    case _ => None
  }

  def zipWith[B, C](s: Stream[B])(f: (A, B) => C): Stream[C] = unfold[C, (Stream[A], Stream[B])]((this, s)) {
    case (Cons(h1, t1), Cons(h2, t2)) => Some(f(h1(), h2()), (t1(), t2()))
    case _ => None
  }

  def zipAll[B](s2: Stream[B]): Stream[(Option[A], Option[B])] =
    unfold((this, s2)){
      case (Cons(h1, t1), Cons(h2, t2)) => Some((Some(h1()), Some(h2())), (t1(), t2()))
      case (Cons(h1, t1), Empty) => Some((Some(h1()), None), (t1(), Empty))
      case (Empty, Cons(h2, t2)) => Some((None, Some(h2())), (Empty, t2()))
      case _ => None
    }

  /**
    * 연습문제 5.14
    */
  def startsWith[A](s: Stream[A]): Boolean = ???

  //
  // Methods added later for testing08.Gen to use
  //

  def zip[B](s2: Stream[B]): Stream[(A,B)] = zipWith(s2)((_,_))

  @annotation.tailrec
  final def find(f: A => Boolean): Option[A] = this match {
    case Empty => None
    case Cons(h, t) => if (f(h())) Some(h()) else t().find(f)
  }
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

  def ones: Stream[Int] = cons(1, ones)

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

  /**
    * 연습문제 5.12
    * unfold를 이용한 fibs, from, constant, ones
    */
  def ones2: Stream[Int] = unfold(1)(s => Some((1, 1)))

  def fibs2(): Stream[Int] = unfold((0, 1)){ case (a1, a2) =>
    Some((a1, (a2, a1 + a2)))
  }

  def constant2[A](a: A): Stream[A] = unfold(a)(_ => Some((a, a)))

  def from2(n: Int): Stream[Int] = unfold(n)(n => Some(n, n + 1))
}