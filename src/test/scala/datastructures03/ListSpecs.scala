package datastructures03

import fpis.datastructures03.List._
import fpis.datastructures03.{List, Nil}
import org.scalatest.FunSuite
import org.scalatest.Matchers._

/**
  * Created by honey.and.sw on 2017. 2. 26.
  */
class ListSpecs extends FunSuite {
  test("drop") {
    val l = List(1, 2, 3, 4, 5)

    drop(l, 0) should equal (List(1, 2, 3, 4, 5))
    drop(l, 2) should equal (List(3, 4, 5))
    drop(l, 5) should equal (List())
    drop(l, 6) should equal (List())

    drop(Nil, 1) should equal (Nil)
  }

  test("dropWhile") {
    val l = List(1, 2, 3, 4, 5)

    dropWhile[Int](l, _ <= 3) should equal (List(4, 5))
    dropWhile[Int](Nil, _ <= 3) should equal (Nil)
    dropWhile[Int](l, _ > 5) should equal (l)
  }

  test("init") {
    init(List(1, 2, 3, 4, 5)) should equal (List(1, 2, 3, 4))
    init(List(1)) should equal (Nil)
  }

  test("reverse") {
    reverse(Nil) should equal (Nil)
    reverse(List(1)) should equal (List(1))
    reverse(List(1, 2, 3)) should equal (List(3, 2, 1))
  }
}
