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
    * 연습문제 3.4
    * 목록에서 처음 n개의 요소를 제거
    */
  def drop[A](l: List[A], n: Int): List[A] = l match {
      case Nil => Nil
      case Cons(h, t) if (n == 0) => l
      case Cons(h, t) if (n > 0) => drop(t, n - 1)
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
  def length[A](as: List[A]): Int = foldRight(as, 0)((a, b) => b + 1)

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
    * 연습문제 3.13
    * foldLeft를 이용한 foldRight
    */
  def foldRight2[A, B](as: List[A], z: B)(f: (A, B) => B): B = foldLeft(as, z)((b, a) => f(a, b))
  def foldLeft2[A, B](as: List[A], z: B)(f: (B, A) => B): B = foldRight(as, z)((a, b) => f(b, a))

  /**
    * 연습문제 3.15
    */
  def append[A](l1: List[A], l2: List[A]): List[A] = l1 match {
    case Nil => l2
    case Cons(x, xs) => Cons(x, append(xs, l2))
  }

  /**
    * 연습문제 3.18
    */
  def map[A, B](as: List[A])(f: A => B): List[B] = foldRight(as, Nil: List[B])((a, b) => Cons(f(a), b))
//  as match {
//    case Nil => Nil
//    case Cons(x, xs) => Cons(f(x), map(xs)(f))
//  }

  /**
    * 연습문제 3.20
    */
  def flatMap[A, B](as: List[A])(f: A => List[B]): List[B] = foldRight(as, Nil: List[B])((a, b) => append(f(a), b))
//  as match {
//    case Nil => Nil
//    case Cons(x, xs) => append(f(x), flatMap(xs)(f))
//  }

}

List.foldRight(List(10, 5, 3, 9,2,3,4,5,6), "tt")((x,y) => x.toString + y)
List.foldRight2(List(10, 5, 3,9,2,3,4,5,6), "tt")((x,y) => x.toString + y)

List.foldRight(List(1, 2, 3), 0)(_ - _)
List.foldRight2(List(1, 2, 3), 0)(_ - _)
