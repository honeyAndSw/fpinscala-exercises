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
    f.stream.take(1).toList should equal (List(4.0))
    f.stream.take(2).toList should equal (List(4.0, 8.0))
    f.stream.take(0).toList should equal (Nil)
    f.stream.take(5).toList should equal (List(4.0, 8.0, 16.0, 32.0))

    f.stream.take2(1).toList should equal (List(4.0))
    f.stream.take2(2).toList should equal (List(4.0, 8.0))
    f.stream.take2(0).toList should equal (Nil)
    f.stream.take2(5).toList should equal (List(4.0, 8.0, 16.0, 32.0))
  }

  test("drop") {
    val f = fixture
    f.stream.drop(2).toList should equal (List(16.0, 32.0))
    f.stream.drop(0).toList should equal (List(4.0, 8.0, 16.0, 32.0))
    f.stream.drop(5).toList should equal (Nil)
  }

  test("takeWhile") {
    val f = fixture
    f.stream.takeWhile(_ >= 4).toList should equal (List(4.0, 8.0, 16.0, 32.0))
    f.stream.takeWhile(_ > 10).toList should equal (Nil)

    f.stream.takeWhile2(_ >= 4).toList should equal (List(4.0, 8.0, 16.0, 32.0))
    f.stream.takeWhile2(_ > 10).toList should equal (Nil)
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

  test("append") {
    val stream2 = Stream.cons(100, Stream.cons(101, Stream.cons(102, Stream.empty)))

    val f = fixture
    f.stream.append(stream2).toList should equal(List(4.0, 8.0, 16.0, 32.0, 100, 101, 102))
  }

  test("constant") {
    val constant = Stream.constant(1)
    constant.take(3).toList should equal (List(1, 1, 1))
  }

  test("from") {
    Stream.from(1).take(5).toList should equal (List(1, 2, 3, 4, 5))
    Stream.from2(1).take(5).toList should equal (List(1, 2, 3, 4, 5))
  }

  test("fibs") {
    Stream.fibs().take(5).toList should equal (List(0, 1, 1, 2, 3))
    Stream.fibs2().take(5).toList should equal (List(0, 1, 1, 2, 3))
  }

  test("unfold") {
    val unfold1 = Stream.unfold(1)(i => Some((i + 1/*다음 값*/, i + 10/*다음 상태*/)))
    unfold1.take(5).toList should equal (List(2, 12, 22, 32, 42))

    val unfold2 = Stream.unfold(1) { i =>
      if (i > 20) None else Some((i + 1, i + 10))
    }
    unfold2.take(5).toList should equal (List(2, 12))
  }

  test("map") {
    fixture.stream.map(d => d - 10).toList should equal (List(-6, -2, 6, 22))
    fixture.stream.map2(d => d - 10).toList should equal (List(-6, -2, 6, 22))
  }

  test("zipWith a stream of the same length") {
    val s1 = Stream.cons(1, Stream.cons(2, Stream.cons(3, Stream.empty)))
    val s2 = Stream.cons(4, Stream.cons(5, Stream.cons(6, Stream.empty)))
    s1.zipAll(s2).toList should equal {
      List((Some(1), Some(4)), (Some(2), Some(5)), (Some(3), Some(6)))
    }
  }

  test("zipWith a stream of different length") {
    val s1 = Stream.cons(1, Stream.cons(2, Stream.cons(3, Stream.empty)))
    val s2 = Stream.cons(4, Stream.cons(5, Stream.empty))
    s1.zipAll(s2).toList should equal {
      List((Some(1), Some(4)), (Some(2), Some(5)), (Some(3), Option.empty))
    }
  }
}
