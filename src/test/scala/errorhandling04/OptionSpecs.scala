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

}
