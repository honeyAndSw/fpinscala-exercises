package streamingio15

import org.scalatest.FunSuite
import org.scalatest.Matchers._

/**
  * Created by honey.and.sw on 2017. 6. 25.
  */
class ProcessSpecs extends FunSuite {
  test("liftOne") {
    val liftP: Process[Int, Int] = Process.liftOne((x: Int) => x * 2)
    val result: List[Int] = liftP(Stream(1, 2, 3)).toList

    result.size should equal (1)
    result.head should equal (1 * 2)
  }

  test("filter") {
    val even: Process[Int, Int] = Process.filter((x: Int) => x % 2 == 0)
    val result = even(Stream(1, 2, 3, 4)).toList

    result.size should equal (2)
  }

  test("sum") {
    val result = Process.sum(Stream(1.0, 2.0, 3.0)).toList
    val result2 = Process.sum2(Stream(1.0, 2.0, 3.0)).toList

    result should equal (List(1.0, 3.0, 6.0))
    result2 should equal (List(1.0, 3.0, 6.0))
  }

}
