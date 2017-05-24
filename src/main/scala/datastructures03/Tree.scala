package datastructures03

sealed trait Tree[+A]
case class Leaf[A](value: A) extends Tree[A]
case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]

object Tree {
  /**
    * 연습문제 3.25
    */
  def size[A](t: Tree[A]): Int = t match {
    case Leaf(v) => 1
    case Branch(l, r) => size(l) + size(r)
  }

  /**
    * 연습문제 3.27
    */
  def depth[A](t: Tree[A]): Int = {

    def call[A](t: Tree[A], accum: Int): Int = t match {
      case Leaf(v) => accum + 1
      case Branch(l, r) => Math.max(call(l, accum + 1), call(r, accum + 1))
    }

    call(t, 0)
  }
}