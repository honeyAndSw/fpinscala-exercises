package monoids10

trait Monoid[A] {
  // 결합법칙을 만족하는 operation
  // op(op(x,y), z) == op(x, op(y,z))
  def op(a1: A, a2: A): A

  def zero: A
}

object Monoid {

  val stringMonoid = new Monoid[String] {
    override def op(a1: String, a2: String): String = a1 + a2
    override def zero: String = ""
  }

  /**
    * 연습문제 10.3
    */
  def endMonoid[A]: Monoid[A => A] = new Monoid[A => A] {
    override def op(a1: A => A, a2: A => A): A => A = a1.compose(a2) // a => a1(a2(a))

    override def zero: A => A = a => a
  }

  def dual[A](m: Monoid[A]): Monoid[A] = new Monoid[A] {
    override def op(a1: A, a2: A): A = m.op(a2, a1)
    override def zero: A = m.zero
  }

  /**
    * 연습문제 10.16
    */
  def productMonoid[A,B](A: Monoid[A], B: Monoid[B]): Monoid[(A, B)] = new Monoid[(A, B)] {
    override def op(a1: (A, B), a2: (A, B)): (A, B) = (A.op(a1._1, a2._1), B.op(a1._2, a2._2))
    override def zero: (A, B) = (A.zero, B.zero)
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
  def foldRight[A,B](as: List[A])(z: B)(f: (A, B) => B): B = {
    // foldMap(as, endMonoid[B])(f.curried)(z)
    val curriedF: A => (B => B) = f.curried
    val foldWithEndMonoid: B => B = foldMap(as, endMonoid[B])(curriedF)
    foldWithEndMonoid(z)
  }

  def foldLeft[A,B](as: List[A])(z: B)(f: (B, A) => B): B = {
    val transformedF: A => (B => B) = a => b => f(b, a)
    val foldWithEndMonoid: B => B = foldMap[A, B => B](as, dual(endMonoid[B]))(transformedF)
    foldWithEndMonoid(z)
  }

  /**
    * 연습문제 10.7
    */
  def foldMapV[A,B](v: IndexedSeq[A], m: Monoid[B])(f: A => B): B =
    if (v.length > 2) {
      val halves: (IndexedSeq[A], IndexedSeq[A]) = v.splitAt(v.length / 2)
      val l: B = foldMapV(halves._1, m)(f)
      val r: B = foldMapV(halves._2, m)(f)
      m.op(l, r)
    } else {
      foldMap(v.toList, m)(f)
    }
}

trait Foldable[F[_]] {
  def foldRight[A,B](as: F[A])(z: B)(f: (A, B) => B): B
  def foldLeft[A,B](as: F[A])(z: B)(f: (B, A) => B): B
  def foldMap[A,B](as: F[A])(f: A => B)(m: Monoid[B]): B
  def concatenate[A](as: F[A])(m: Monoid[A]): A = foldLeft(as)(m.zero)(m.op)

  /**
    * 연습문제 10.15
    */
  def toList[A](as: F[A]): List[A] = foldRight(as)(List[A]())((a, acc) => a +: acc)
}

object Foldable {

  /**
    * 연습문제 10.12
    */
  object FoldableList extends Foldable[List] {
    override def foldRight[A, B](as: List[A])(z: B)(f: (A, B) => B): B = as.foldRight(z)(f)

    override def foldLeft[A, B](as: List[A])(z: B)(f: (B, A) => B): B = as.foldLeft(z)(f)

    override def foldMap[A, B](as: List[A])(f: (A) => B)(m: Monoid[B]): B =
      foldRight(as)(m.zero)((a, acc) => m.op(f(a), acc))
  }
}
