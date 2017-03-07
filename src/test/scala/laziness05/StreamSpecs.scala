package laziness05

import org.scalatest.FunSuite
import org.scalatest.Matchers._

import scala.collection.immutable.List

class StreamSpecs extends FunSuite {

  def fixture = new {
    val i = {
      println("print i...") // println is done only once.
      2
    }
    val stream = Stream.apply(
      Math.pow(i, 2), Math.pow(i, 3), Math.pow(i, 4), Math.pow(i, 5)
    )
  }

  test("toList") {
    val f = fixture
    f.stream.toList should equal (List(4.0, 8.0, 16.0, 32.0))
  }

  test("take") {
    val f = fixture
    f.stream.take(2).toList should equal (List(4.0, 8.0))
    f.stream.take(0).toList should equal (Nil)
    f.stream.take(5).toList should equal (List(4.0, 8.0, 16.0, 32.0))
  }

  test("forAll") {
    val f = fixture
    f.stream.forAll(d => d < 10.0) should equal (false)
    f.stream.forAll(d => d < 33.0) should equal (true)
  }

}
