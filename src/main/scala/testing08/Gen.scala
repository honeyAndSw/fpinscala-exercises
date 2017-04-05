package testing08

import fpinscala.testing.RNG.Rand
import fpinscala.testing.{RNG, State}
import laziness05.Stream
import testing08.Prop._

// p.104 계산된 다음 상태를 프로그램의 나머지 부분에 전달하는 책임을 호출자에게 지우는 것
// State[RNG, A]는 Rand를 감싼 것과 동일하다.
// case class State[S, +A](run: S => (A, S))

case class Gen[A](sample: State[RNG, A]) {
  /**
    * 연습문제 8.6
    */
  def flatMap[B](f: A => Gen[B]): Gen[B] = {
    val s: State[RNG, B] = sample.flatMap(a => f(a).sample)
    Gen(s)
  }

  def listOfNViaFlatMap(size: Gen[Int]): Gen[List[A]] =
    size.flatMap { s => Gen.listOfN(s, this) }

  /**
    * 연습문제 8.10
    */
  def unsized: SGen[A] = SGen(n => this)
}

// 검례 최소화를 위해 크기별로 Gen을 생성
case class SGen[A](forSize: Int => Gen[A])

object Gen {
  // type Rand[+A] = RNG => (A, RNG)

  /**
    * 연습문제 8.4
    */
  def choose(start: Int, stopExclusive: Int): Gen[Int] = {
    val rand: Rand[Int] = RNG.map(RNG.nonNegativeInt) { n =>
      start + (n % (stopExclusive - start))
    }

    Gen(State(rand))
  }

  /**
    * 연습문제 8.5
    */
  def unit[A](a: => A): Gen[A] = {
    val rand: Rand[A] = RNG.unit(a)
    Gen(State(rand))
  }

  def boolean: Gen[Boolean] = Gen(State { rng =>
      val (i, r) = rng.nextInt
      (i % 2 == 0, r)
    })

  def listOfN[A](n: Int, g: Gen[A]): Gen[List[A]] = {
    // p.108 연습문제 6.7
    val list: List[State[RNG, A]] = List.fill(n)(g.sample)
    Gen(State.sequence(list))
  }
  /* 연습문제 8.5 */

  /**
    * 연습문제 8.12
    */
  def listOf[A](g: Gen[A]): SGen[List[A]] = SGen(n => Gen.listOfN(n, g))

  /**
    * 연습문제 8.13
    * test("Prop of List.max2")
    */
  def listOf1[A](g: Gen[A]): SGen[List[A]] = SGen(n => Gen.listOfN(n max 1, g))
}

/* -------------------- Prop -------------------- */

object Prop {
  type TestCases = Int
  type FailedCase = String
  type SuccessCount = Int
  type MaxSize = Int

  // Some(테스트 최종 상태) => 테스트 실패
  // None => 테스트 성공, 테스트 실패가 존재하지 않음
  // type Result = Option[(FailedCase, SuccessCount)]
  sealed trait Result {
    def isFalsified: Boolean
  }

  case object Passed extends Result {
    override def isFalsified: Boolean = false
  }

  case object Proved extends Result {
    override def isFalsified: Boolean = false
  }

  case class Falsified(failure: FailedCase, successes: SuccessCount) extends Result {
    override def isFalsified: Boolean = true
  }

  def forAll[A](as: Gen[A])(f: A => Boolean): Prop = Prop { (max, n, rng) =>
    randomStream(as)(rng)
      .zip(Stream.from(0)) // (a, i) Stream 생성
      .take(n)
      .map { case (a, i) =>
        try {
          if (f(a)) Passed else Falsified(a.toString, i)
        } catch { case e: Exception => Falsified(buildMsg(a, e), i) }
      }
      .find(_.isFalsified)
      .getOrElse(Passed)
  }

  def randomStream[A](g: Gen[A])(rng: RNG): Stream[A] =
    Stream.unfold(rng)(rng => Some(g.sample.run(rng)))

  private def buildMsg[A](s: A, e: Exception): String =
    s"""
      |test case: ${s}
      |generated an exception: ${e.getMessage}
      |stack trace:\n ${e.getStackTrace}
    """.stripMargin

  def forAll[A](a: SGen[A])(f: A => Boolean): Prop = forAll(a.forSize)(f)

  def forAll[A](g: Int => Gen[A])(f: A => Boolean): Prop = Prop {
    (max,n,rng) =>
      val casesPerSize = (n - 1) / max + 1
      val props: Stream[Prop] =
        Stream.from(0).take((n min max) + 1).map(i => forAll(g(i))(f))
      val prop: Prop =
        props.map(p => Prop { (max, n, rng) =>
          p.run(max, casesPerSize, rng)
        }).toList.reduce(_ && _)
      prop.run(max,n,rng)
  }

  def run(p: Prop,
          maxSize: Int = 100,
          testCases: Int = 100,
          rng: RNG = RNG.Simple(System.currentTimeMillis)): Unit =
    p.run(maxSize, testCases, rng) match {
      case Falsified(msg, n) => println(s"! Falsified after $n passed tests:\n $msg")
      case Passed            => println(s"+ OK, passed $testCases tests.")
      case Proved            => println(s"+ OK, proved property.")
    }

  def check(p: => Boolean): Prop = Prop { (_, _, _) =>
    // Passed를 사용하면 run에서 적절한 메시지를 출력할 수 없다.
    // if (p) Passed
    if (p) Proved
    else Falsified("()", 0)
  }
// 변하지 않는 속성 p를 여러번 반복하는 비효율적인 방법
//  {
//    lazy val result = p
//    forAll(Gen.unit())(_ => result)
//  }
}

// TestCases 검례 개수
// RNG 무작위의 데이터 생성을 위한 원천 object
case class Prop(run: (MaxSize, TestCases, RNG) => Result) {

//  연습문제 8.3
//  def check: Either[(FailedCase, SuccessCount), SuccessCount] = ???
//  def &&(p: Prop): Prop = {
//    val c1 = check
//
//    new Prop {
//      override def check: Boolean = c1 && p.check
//    }
//  }

  /**
    * 연습문제 8.9
    */
  def &&(p: Prop): Prop = Prop { (max, n, rng) =>
    p.run(max, n, rng) match {
      case Passed | Proved => run(max, n, rng)
      case f => f
    }
  }

  def ||(p: Prop): Prop = Prop { (max, n, rng) =>
    p.run(max, n, rng) match {
      case Falsified(_, _) => run(max, n, rng)
      case s => s
    }
  }
  /* 연습문제 8.9 */
}
