package fpis.errorhandling04

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