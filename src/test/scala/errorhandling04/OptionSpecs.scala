package errorhandling04

import fpis.errorhandling04.Option
import fpis.errorhandling04.Some
import fpis.errorhandling04.None
import org.scalatest.FunSuite
import org.scalatest.Matchers._

/**
  * OptionSpecs
  */
class OptionSpecs extends FunSuite {
  private val some: Option[Int] = Some(1)
  private val none: Option[Int] = None

  test("map") {
    some.map(_ + 1) should equal (Some(2))
    none.map(_ + 1) should equal (None)
  }

  test("flatMap") {
    some.flatMap(i => Some(i + 1)) should equal (Some(2))
    some.flatMap(i => None) should equal (None)
    none.flatMap(i => Some(i + 1)) should equal (None)
  }

  test("getOrElse") {
    some.getOrElse(10) should equal (1)
    none.getOrElse(10) should equal (10)
  }

  test("orElse") {
    some.orElse(Some(2)) should equal (some)
    none.orElse(Some(2)) should equal (Some(2))
  }

  test("filter") {
    some.filter(_ % 2 == 0) should equal (None)
    some.filter(_ % 2 == 1) should equal (some)
    none.filter(_ % 2 == 1) should equal (None)
  }

  test("map2") {
    Option.map2[Int, Int, Int](Some(1), Some(2))(_ + _) should equal (Some(3))
    Option.map2[Int, Int, Int](Some(1), None)(_ + _) should equal (None)
    Option.map2[Int, Int, Int](None, Some(2))(_ + _) should equal (None)
    Option.map2[Int, Int, Int](None, None)(_ + _) should equal (None)
  }

  test("sequence") {
    val l1 = List(Some(1), Some(2), Some(3))
    Option.sequence(l1) should equal {
      Some(List(1, 2, 3))
    }

    Option.sequence(List(None)) should equal (None)
    Option.sequence(List(Some(1), None, Some(3))) should equal (None)
    Option.sequence(List()) should equal (Some(List()))
  }

  test("traverse") {
    def divide10By(a: Int): Option[Int] = {
      if (a == 0) None else Some(10 / a)
    }

    Option.traverse(List(2, 5))(divide10By) should equal {
      Some(List(5, 2))
    }

    Option.traverse(Nil: List[Int])(divide10By) should equal (Some(Nil))
    Option.traverse(List(0, 2))(divide10By) should equal (None)
    Option.traverse(List(2, 0, 5))(divide10By) should equal (None)
  }

  test("sequence2") {
    val l1 = List(Some(1), Some(2), Some(3))
    Option.sequence2(l1) should equal {
      Some(List(1, 2, 3))
    }

    Option.sequence2(List(None)) should equal (None)
    Option.sequence2(List(Some(1), None, Some(3))) should equal (None)
    Option.sequence2(List()) should equal (Some(List()))
  }

}
