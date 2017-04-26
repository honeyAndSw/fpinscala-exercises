package testing08

import java.util.concurrent.{Executors, ExecutorService}

import org.scalatest.FunSuite
import parallelism07.Par

/**
  * GenSpecs
  *
  * @author naheon
  * @since 2017. 04. 12.
  */
class GenSpecs extends FunSuite {
  test("Prop of List.max") {
    val smallInt: Gen[Int] = Gen.choose(-10, 10)
    val maxProp: Prop = Prop.forAll(Gen.listOf(smallInt)) { ns: List[Int] =>
      val max = ns.max
      !ns.exists(_ > max)
    }

    // val result = maxProp.run(100, 100, RNG.Simple(System.currentTimeMillis()))
    // println(result)
    Prop.run(maxProp)
  }

  /**
    * 연습문제 8.13
    */
  test("Prop of List.max2") {
    val smallInt: Gen[Int] = Gen.choose(-10, 10)
    val maxProp: Prop = Prop.forAll(Gen.listOf1(smallInt)) { ns: List[Int] =>
      val max = ns.max
      !ns.exists(_ > max)
    }

    Prop.run(maxProp)
  }

  test("Prop of Par") {
    val es: ExecutorService = Executors.newCachedThreadPool

    // 검사의 의도가 명확하게 드러나지 않는다.
    val p1 = Prop.forAll(Gen.unit(Par.unit(1))) { i =>
      // Par.map(i)(_ + 1) == Par.unit(2)
      Par.map(i)(_ + 1)(es).get == Par.unit(2)(es).get
    }
    // Prop.run(p1)

    // 한 번의 검사에 의해 증명됨을 나타낼 수 있는 Prop.check를 추가
    val p2 = Prop.check {
      val l = Par.map(Par.unit(1))(_ + 1)
      val r = Par.unit(2)
      // Par의 내부 구현 세부사항을 최소한으로 사용하는 것이 바람직하다.
      l(es).get == r(es).get
    }
    // Prop.run(p2)

    val p3 = Prop.check {
      Par.equal(
        Par.map(Par.unit(1))(_ + 1),
        Par.unit(2)
      )(es).get
    }
  }
}
