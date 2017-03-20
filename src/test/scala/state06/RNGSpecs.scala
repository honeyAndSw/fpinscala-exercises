package state06

import state06.RNG.SimpleRNG
import org.scalatest.FunSuite
import org.scalatest.Matchers._

/**
  * Created by honey.and.sw on 2017. 3. 19.
  */
class RngSpecs extends FunSuite {

  def fixture = new {
    val rng1 = SimpleRNG(10)
    val rng2 = SimpleRNG(15)
  }

  test("double") {
    val rng = fixture.rng1
    RNG.double(rng)._1 should (be >= 0.0 and be < 1.0)
    RNG.doubleWithMap(rng)._1 should (be >= 0.0 and be < 1.0)
  }

  test("ints") {
    val (list, r) = RNG.ints(5)(fixture.rng1)
    println(s"${list}")
    list.size should equal (5)

    val (list2, r2) = RNG.ints2(5)(fixture.rng1)
    println(s"${list2}")
    list2.size should equal (5)
  }

  test("more ints") {
    val (list, _) = RNG.intsWithSequence(5)(fixture.rng1)
    println(s"${list}")
    list.size should equal (5)
  }
}
