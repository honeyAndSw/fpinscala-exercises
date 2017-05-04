package monoids10

import org.scalatest.FunSuite
import org.scalatest.Matchers._

/**
  * MonoidSpecs
  *
  * @author naheon
  * @since 2017. 05. 04.
  */
class MonoidSpecs extends FunSuite {

  test("fold via foldMap") {
    val input = List("a", "b", "c", "d", "e")

    Monoid.foldRight(input)("")(_ + _) should equal (input.foldRight("")(_ + _))
    Monoid.foldLeft(input)("")(_ + _) should equal (input.foldLeft("")(_ + _))
  }

  test("foldMapV") {
    val input = IndexedSeq("a", "b", "c", "d", "e")
    Monoid.foldMapV(input, Monoid.stringMonoid)(_.self) should equal (input.foldLeft("")(_ + _))

    val small = IndexedSeq("a", "b")
    Monoid.foldMapV(small, Monoid.stringMonoid)(_.self) should equal (small.foldLeft("")(_ + _))

    val empty = IndexedSeq[String]()
    Monoid.foldMapV(empty, Monoid.stringMonoid)(_.self) should equal (empty.foldLeft("")(_ + _))
  }

  test("FoldableList") {
    val input = List("a", "b", "c", "d", "e")
    val fold: String = input.foldLeft("")(_ + _)

    Foldable.FoldableList.foldRight(input)("")(_ + _) should equal (fold)
    Foldable.FoldableList.toList(input) should equal (input)
  }
}
