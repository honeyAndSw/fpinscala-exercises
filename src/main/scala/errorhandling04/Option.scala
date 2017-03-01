package fpis.errorhandling04

sealed trait Option[+A] {
  /***************
    * 연습문제 4.1 *
    **************/
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

  /**
    * 연습문제 4.2
    */
  def variance(xs: Seq[Double]): Option[Double] =
    Some(xs)
      .filter(_.length > 0)
      .flatMap { seq =>
        val m = seq.foldLeft(0.0)(_ + _) / seq.length
        val m2 = seq.map(d => math.pow(d - m, 2)).foldLeft(0.0)(_ + _) / seq.length
        Some(m2)
      }

}