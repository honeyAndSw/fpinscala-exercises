package testing08

import fpinscala.testing.RNG
import fpinscala.testing.RNG.Rand
import fpinscala.testing.State
import testing08.Prop.{FailedCase, SuccessCount}

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
}

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

  // p.108 연습문제 6.7
  def listOfN[A](n: Int, g: Gen[A]): Gen[List[A]] = {
    val list: List[State[RNG, A]] = List.fill(n)(g.sample)
    Gen(State.sequence(list))
  }
  /* 연습문제 8.5 */

}

trait Prop {

  def check: Either[(FailedCase, SuccessCount), SuccessCount]

  /**
   * 연습문제 8-3
  def &&(p: Prop): Prop = {
    val c1 = check

    new Prop {
      override def check: Boolean = c1 && p.check
    }
  }
  */
}

object Prop {
  type FailedCase = String
  type SuccessCount = Int
}