package fpis.errorhandling04

/********************************
 *          연습문제 4.1          *
 ********************************/
sealed trait Option[+A] {
  def map[B](f: A => B): Option[B] = this match {
    case Some(get) => Some(f(get))
    case _ => None
  }

  def flatMap[B](f: A => Option[B]): Option[B] = map(f).getOrElse(None)

  def getOrElse[B >: A](default: => B): B = this match {
    case Some(get) => get
    case _ => default
  }

  def orElse[B >: A](ob: => Option[B]): Option[B] = map(Some(_)).getOrElse(ob)

  def filter(f: A => Boolean): Option[A] = flatMap(a => if (f(a)) this else None)

}

case class Some[+A](get: A) extends Option[A]
case object None extends Option[Nothing]

object Option {
  def Try[A](a: => A): Option[A] =
    try Some(a)
    catch { case e: Exception => None }

  /**
    * 연습문제 4.2
    */
  def variance(xs: Seq[Double]): Option[Double] =
    Some(xs)
      .filter(_.length > 0)
      .flatMap { seq =>
        val m = /* seq.foldLeft(0.0)(_ + _) */ seq.sum / seq.length
        val m2 = seq.map(d => math.pow(d - m, 2)).foldLeft(0.0)(_ + _) / seq.length
        Some(m2)
      }

  /**
    * 연습문제 4.3
    */
  def map2[A, B, C](a: Option[A], b: Option[B])(f: (A, B) => C): Option[C] =
    a.flatMap { a1 =>
      b.map{ b1 => f(a1, b1) }
    }

  /**
    * 연습문제 4.4
    */
  def sequence[A](a: List[Option[A]]): Option[List[A]] = a match {
    case Nil => Some(Nil)
    case _ => map2(a.head, sequence(a.tail)){
      (h1: A, seq1: List[A]) => h1 +: seq1
    }
  }

  /**
    * 연습문제 4.5
    * 목록을 단 한 번만 훑는 접근
    */
  def traverse[A, B](a: List[A])(f: A => Option[B]): Option[List[B]] = {
    a.foldRight[Option[List[B]]](Some(Nil)) {
      (a1: A, acc: Option[List[B]]) => map2(f(a1), acc)(_ +: _)
    }
  }

  def sequence2[A](a: List[Option[A]]): Option[List[A]] = traverse[Option[A], A](a)(identity)
}