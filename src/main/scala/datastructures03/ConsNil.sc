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
}

var l1 = List(1, 2, 3, 4, 5)
var l2 = List.init(l1)