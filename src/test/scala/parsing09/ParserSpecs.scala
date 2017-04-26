package parsing09

import org.scalatest.Matchers._
import org.scalatest.{Assertion, Outcome, fixture}
import parsing09.Parsers.Parser

/**
  * Created by honey.and.sw on 2017. 4. 29.
  */
class ParserSpecs extends fixture.FunSuite {
  test("regex") { f =>
    val result: Result[String] = f.run(f.regex("abra".r))("abra cadabra")

    assertParseSuccess[String](result, suc => {
      suc.get should equal ("abra")
      suc.charsConsumed should equal ("abra".length)
    })
  }

  private def assertParseSuccess[A](result: Result[A], assert: Success[A] => Assertion) = {
    result match {
      case Failure(_, _) => fail()
      case s @ Success(_, _) => assert(s)
    }
  }

  override protected def withFixture(test: OneArgTest): Outcome = {
    val f = new Parsers {
      override def listOfN[A](n: Int, p: Parser[A]): Parser[List[A]] = ???

      override def run[A](p: Parser[A])(input: String): Result[A] = p(Location(input, 0))
    }

    test(f)
  }

  override type FixtureParam = Parsers
}
