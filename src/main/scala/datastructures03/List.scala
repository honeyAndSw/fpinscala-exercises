package fpis.datastructures03

sealed trait List[+A]
case object Nil extends List[Nothing]
case class Cons[+A](head: A, tail: List[A]) extends List[A]

object List {

  def apply[A](as: A*): List[A] = {
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))
  }

  /**
    * 연습문제 3.2
    * List의 첫 요소를 제거
    */
  def tail[A](l: List[A]): List[A] = l match {
    case Nil => Nil
    case Cons(h, t) => t
  }

  /**
    * 연습문제 3.3
    * List의 첫 요소를 다른 값으로 대체
    */
  def setHead[A](l: List[A], a: A): List[A] = l match {
    case Nil => l
    case Cons(_, xs) => Cons(a, xs)
  }

  /**
    * 연습문제 3.4
    * 목록에서 처음 n개의 요소를 제거
    */
  def drop[A](l: List[A], n: Int): List[A] = l match {
      case Nil => Nil
      case Cons(h, t) if (n == 0) => l
      case Cons(h, t) if (n > 0) => drop(t, n - 1)
    }

  /**
    * 연습문제 3.5
    */
  def dropWhile[A](l: List[A], f: A => Boolean): List[A] = l match {
    case Cons(x, xs) if f(x) => dropWhile(xs, f)
    case _ => l
  }

  /**
    * 연습문제 3.6
    * 마지막 요소를 제외한 모든 요소로 이루어진 List를 리턴
    */
  def init[A](l: List[A]): List[A] = l match {
    case Nil => Nil
    case Cons(h, Nil) => Nil // equals to l.length = 1
    case Cons(h, t) => Cons(h, init(t))
  }

  def foldRight[A, B](as: List[A], z: B)(f: (A, B) => B): B = {
    as match {
      case Nil => z
      case Cons(x, xs) => f(x, foldRight(xs, z)(f)) // 꼬리재귀가 아님!
    }
  }

  /**
    * 연습문제 3.9
    */
  def length[A](as: List[A]): Int = foldRight(as, 0)((_, acc) => acc + 1)

  /**
    * 연습문제 3.10
    * 꼬리재귀가 있는 foldLeft
    */
  def foldLeft[A, B](as: List[A], z: B)(f: (B, A) => B): B = {
    as match {
      case Nil => z
      case Cons(x, xs) => foldLeft(xs, f(z, x))(f)
    }
  }

  /**
    * 연습문제 3.11
    * foldLeft를 이용한 sum, product, length
    */
  def sum2(as: List[Int]): Int = foldLeft(as, 0)(_ + _)
  def product2(as: List[Double]): Double = foldLeft(as, 1.0)(_ * _)
  def length2[A](as: List[A]): Int = foldLeft(as, 0)((acc, _) => acc + 1)

  /**
    * 연습문제 3.12
    */
  def reverse[A](as: List[A]): List[A] = foldLeft(as, Nil: List[A])((acc, a) => Cons(a, acc))

  /**
    * 연습문제 3.13
    * foldLeft를 이용한 foldRight
    */
  def foldRight2[A, B](as: List[A], z: B)(f: (A, B) => B): B = foldLeft(reverse(as), z)((acc, a) => f(a, acc))
  def foldLeft2[A, B](as: List[A], z: B)(f: (B, A) => B): B = foldRight(reverse(as), z)((a, acc) => f(acc, a))

  def append[A](l1: List[A], l2: List[A]): List[A] = l1 match {
    case Nil => l2
    case Cons(x, xs) => Cons(x, append(xs, l2))
  }

  /**
    * 연습문제 3.14
    * foldLeft나 foldRight를 이용한 append
    */
  def append2[A](l1: List[A], l2: List[A]): List[A] = foldRight(l1, l2)((a, acc) => Cons(a, acc))
  def append3[A](l1: List[A], l2: List[A]): List[A] = foldLeft(reverse(l1), l2)((acc, a) => Cons(a, acc))

  /**
    * 연습문제 3.15
    * 목록들의 목록을 하나의 목록으로 연결
    */
  def concat[A](as: List[List[A]]): List[A] = foldLeft(as, Nil: List[A])((acc, a) => append(acc, a))

  /**
    * 연습문제 3.16
    */
  def increase(as: List[Int]): List[Int] = foldRight(as, Nil: List[Int])((a, acc) => Cons(a + 1, acc))

  /**
    * 연습문제 3.17
    */
  def toString(as: List[Double]): List[String] = foldRight(as, Nil: List[String])((a, acc) => Cons(a.toString, acc))

  /**
    * 연습문제 3.18
    * 목록의 구조를 유지하면서 목록의 각 요소를 수정하는 일반적인 함수
    */
  def map[A, B](as: List[A])(f: A => B): List[B] = foldRight(as, Nil: List[B])((a, b) => Cons(f(a), b))

  /**
    * 연습문제 3.19
    * Predicate를 만족하는 요소들을 제거하는 함수
    */
  def filter[A](as: List[A])(f: A => Boolean): List[A] =
    foldRight(as, Nil: List[A]){(a, acc) => if (f(a)) acc else Cons(a, acc)}

  /**
    * 연습문제 3.20
    */
  def flatMap[A, B](as: List[A])(f: A => List[B]): List[B] = foldRight(as, Nil: List[B])((a, acc) => append(f(a), acc))

  /**
    * 연습문제 3.21
    * flatMap을 이용한 filter
    */
  def filter2[A](as: List[A])(f: A => Boolean): List[A] = flatMap(as)(a => if (f(a)) Nil else List(a))

  /**
    * 연습문제 3.22
    * 대응되는 요소들을 더한 값들로 이루어진 새 목록
    */
  def zipInt(l1: List[Int], l2: List[Int]): List[Int] = (l1, l2) match {
    case (Cons(h1, t1), Cons(h2, t2)) => Cons(h1 + h2, zipInt(t1, t2))
    case (_, _) => Nil
  }

  /**
    * 연습문제 3.23
    */
  def zipWith[A, B, C](l1: List[A], l2: List[B])(f: (A, B) => C): List[C] = (l1, l2) match {
    case (Cons(h1, t1), Cons(h2, t2)) => Cons(f(h1, h2), zipWith(t1, t2)(f))
    case (_, _) => Nil
  }
}