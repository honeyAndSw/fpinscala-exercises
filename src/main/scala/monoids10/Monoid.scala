package monoids10

trait Monoid[A] {
  // 결합법칙을 만족하는 operation
  // op(op(x,y), z) == op(x, op(y,z))
  def op(a1: A, a2: A): A

  def zero: A
}

object Monoid {

  /**
    * 연습문제 10.3
    */
  def endMonoid[A]: Monoid[A => A] = new Monoid[A => A] {
    override def op(a1: A => A, a2: A => A): A => A = a1.compose(a2) // a => a2(a1(a))

    override def zero: A => A = a => a
  }

  def concatenate[A](as: List[A], m: Monoid[A]): A = as.foldLeft(m.zero)(m.op)

  /**
    * 연습문제 10.5
    */
  def foldMap[A,B](as: List[A], m: Monoid[B])(f: A => B): B =
    as.foldLeft(m.zero)((acc, a) => m.op(acc, f(a)))

  /**
    * 연습문제 10.6
    */
  def foldLeft[A,B](as: List[A])(z: B)(f: (A, B) => B): B = foldMap(as, endMonoid[B])(f.curried)(z)

  def foldRight[A,B](as: List[A])(z: B)(f: (B, A) => B): B = ???

}
