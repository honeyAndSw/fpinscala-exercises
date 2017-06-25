package localeffects14

import org.scalatest.FunSuite

/**
  * Created by honey.and.sw on 2017. 6. 18.
  */
class StateTransitionSpecs extends FunSuite {
  test("STRef") {
    val a: ST[Nothing, (Int, Int)] = for {
      r1 <- STRef[Nothing, Int](1)
      r2 <- STRef[Nothing, Int](2)
      x <- r1.read
      y <- r2.read
      _ <- r1.write(y + 1)
      _ <- r2.write(x + 1)
      a <- r1.read
      b <- r2.read
    } yield (a, b)
  }

  test("RunnableST") {
    val a = new RunnableST[(Int, Int)] {
      override def apply[S]: ST[S, (Int, Int)] = for {
        r1 <- STRef(1)
        r2 <- STRef(2)
        x <- r1.read
        y <- r2.read
        _ <- r1.write(y + 1)
        _ <- r2.write(x + 1)
        a <- r1.read
        b <- r2.read
      } yield (a, b)
    }
  }

  test("RunnableST - 2") {
//    val a = new RunnableST[STRef[Nothing, Int]] {
//      override def apply[S]: ST[S, STRef[Nothing, Int]] = STRef(1)
//    }
  }

}
