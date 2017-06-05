package iomonad13

import org.scalatest.FunSuite

/**
  * Created by honey.and.sw on 2017. 6. 6.
  */
class IOSpecs extends FunSuite {
  test("IO2 - forever") {
    import iomonad13.IO2.{IO, PrintLine}
    val p: IO[Unit] = IO.forever(PrintLine("Still going..."))
    p.run
  }

  test("IO3 - forever") {
    import iomonad13.IOTailRec.{TailRec, printLine}
    val p: TailRec[Unit] = TailRec.forever(printLine("Still going..."))
  }

  test("Stack Overflow") {
    import iomonad13.IOTailRec.{TailRec, Return, Suspend}

    val f: (Int => Int) = (x: Int) => x
    val g = List.fill(10000)(f).foldLeft(f)(_ compose _)

    val f1: (Int => TailRec[Int]) = (x: Int) => Return(x)
    val g1 = List.fill(10000)(f1).foldLeft(f1) { (acc, f) =>
      (x: Int) => Suspend(() => ()).flatMap {
        _ => acc(x).flatMap(f)
      }
    }
  }
}
