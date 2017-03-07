package laziness05

import org.scalatest.FunSuite
import org.scalatest.Matchers._

import scala.collection.immutable.List

class StreamSpecs extends FunSuite {

  def fixture = new {
    def pow(i: Int, p: Int): Double = {
      // println(s"pow ${i}, ${p}")
      Math.pow(i, p)
    }

    val i = {
      println("print i...") // println is done only once.
      2
    }

    // Stream of Math.pow(i, 2), Math.pow(i, 3), Math.pow(i, 4), Math.pow(i, 5)
    val stream = Stream.cons(
      pow(i, 2), Stream.cons(
        pow(i, 3), Stream.cons(
          pow(i, 4), Stream.cons(
            pow(i, 5), Stream.empty)))
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

  test("drop") {
    val f = fixture
    f.stream.drop(2).toList should equal (List(16.0, 32.0))
    f.stream.drop(0).toList should equal (List(4.0, 8.0, 16.0, 32.0))
    f.stream.drop(5).toList should equal (Nil)
  }

  test("takeWhile") {
    val f = fixture
    f.stream.takeWhile(_ >= 4).toList should equal (List(4.0))
    f.stream.takeWhile(_ > 10).toList should equal (List(4.0, 8.0, 16.0))
  }

  test("forAll") {
    val f = fixture
    f.stream.forAll(d => d < 10.0) should equal (false)
    f.stream.forAll(d => d < 33.0) should equal (true)
  }

  test("headOption") {
    val f = fixture
    f.stream.headOption should equal (Some(4.0))
    Stream.empty.headOption should equal (None)
  }

  test("headOption2") {
    val f = fixture
    f.stream.headOption2 should equal (Some(4.0))
    Stream.empty.headOption2 should equal (None)
  }

  test("constant") {
    val constant = Stream.constant(1)
    constant.take(3).toList should equal (List(1, 1, 1))
  }

  test("from") {
    val from = Stream.from(1)
    from.take(5).toList should equal (List(1, 2, 3, 4, 5))
  }

  test("fibs") {
    Stream.fibs().take(5).toList should equal (List(0, 1, 1, 2, 3))
  }

  test("unfold") {
    val unfold1 = Stream.unfold(1)(i => Some((i + 1/*다음 값*/, i + 10/*다음 상태*/)))
    unfold1.take(5).toList should equal (List(2, 12, 22, 32, 42))

    val unfold2 = Stream.unfold(1) { i =>
      if (i > 20) None else Some((i + 1, i + 10))
    }
    unfold2.take(5).toList should equal (List(2, 12))
  }
}
