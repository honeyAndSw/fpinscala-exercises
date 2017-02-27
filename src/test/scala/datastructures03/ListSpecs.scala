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

  test("foldRight implemented with foldLeft") {
    val fr1 = foldRight[Char, String](List('a', 'b', 'c', 'd', 'e'), "0")((a: Char, acc: String) => a.toString + acc)
    val fr2 = foldRight2[Char, String](List('a', 'b', 'c', 'd', 'e'), "0")((a: Char, acc: String) => a.toString + acc)

    fr2 should equal (fr1)
  }

  test("foldLeft implemented with foldRight") {
    val fl1 = foldLeft[Char, String](List('a', 'b', 'c', 'd', 'e'), "0")((acc: String, a: Char) => acc + a.toString)
    val fl2 = foldLeft2[Char, String](List('a', 'b', 'c', 'd', 'e'), "0")((acc: String, a: Char) => acc + a.toString)

    fl2 should equal (fl1)
  }

  test("append") {
    append(List('a', 'b', 'c'), Nil) should equal (List('a', 'b', 'c'))
    append(Nil, List('d', 'e', 'f')) should equal (List('d', 'e', 'f'))
    append(List('a', 'b', 'c'), List('d', 'e', 'f')) should equal (List('a', 'b', 'c', 'd', 'e', 'f'))
  }

  test("append implemented with foldRight") {
    val a1 = append(List('a', 'b', 'c'), List('d', 'e', 'f'))
    val a2 = append2(List('a', 'b', 'c'), List('d', 'e', 'f'))
    val a3 = append3(List('a', 'b', 'c'), List('d', 'e', 'f'))
    a2 should equal (a1)
    a3 should equal (a1)
  }

  test("concat") {
    val c = concat(List(List('a', 'b', 'c'), List('d', 'e', 'f'), List('g', 'h', 'i')))
    c should equal (List('a', 'b', 'c', 'd', 'e', 'f', 'g', 'h', 'i'))
  }
}
