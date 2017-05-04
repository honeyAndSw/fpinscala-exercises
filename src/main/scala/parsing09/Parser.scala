package parsing09

import parsing09.Parsers.Parser
import testing08._

import scala.util.matching.Regex

object Parsers {
  type Parser[+A] = Location => Result[A]
}

trait Parsers/*[Parser[+_]]*/ { self =>
  def run[A](p: Parser[A])(input: String): Result[A]

  /**
    * 연습문제 9.13
    */
  // run(string(s))(s) == Right(s)
  implicit def string(s: String): Parser[String] = loc => {
    if (loc.input.startsWith(s)) Success(s, s.length)
    else Failure(loc.toError(s"Expected: ${s}"), true)
  }

  /**
    * 연습문제 9.13
    */
  implicit def regex(r: Regex): Parser[String] = loc => {
    r.findFirstMatchIn(loc.input) match {
      case Some(m) => Success(loc.input.substring(m.start, m.end), m.end)
      case _ => Failure(loc.toError(s"Expected: ${r}"), true)
    }
  }

  implicit def asStringParser[A](a: A)(implicit f: A => Parser[String]): ParserOps[String] = ParserOps(f(a))

  implicit def operators[A](p: Parser[A]) = ParserOps[A](p)

  // run(char(c))(c.toString) == Right(c)
  def char(c: Char): Parser[Char] = string(c.toString) map (_.charAt(0))

  // run(or(string("abra"), string("cadabra")))("abra") == Right("abra")
  // run(or(string("abra"), string("cadabra")))("cadabra") == Right("cadabra")
  def or[A](s1: Parser[A], s2: => Parser[A]): Parser[A] = loc => {
    s1(loc) match {
      case Failure(e, false) => s2(loc)
      case r => r // s1(loc)의 결과를 그대로 넘긴다.
    }
  }

  // 파싱 확정(해당 파싱을 끝까지 실행하기로 결정함)을 지연하는 조합기
  // attempt(p flatMap (_ => fail)) or p2 == p2
  def attempt[A](p: Parser[A]): Parser[A] = loc => p(loc).uncommit

  def listOfN[A](n: Int, p: Parser[A]): Parser[List[A]]

  /**
    * 연습문제 9.13
    *
    * 입력 문자열 중 파서가 조사한 조각만 돌려주는 파서
    * run(slice(('a'|'b').many))("aaba") == Right("aaba")
    */
  def slice[A](p: Parser[A]): Parser[String] = loc => {

    def exec(loc: Location, out: String): Result[String] = {
      p(loc) match {
        case Success(a, charsConsumed) =>
          val totalConsumed = loc.offset + charsConsumed
          exec(Location(loc.input.substring(totalConsumed), totalConsumed), out + a)
        case f @ Failure(_, _) => f
      }
    }

    exec(loc, "")
  }

  /**
    * 연습문제 9.3
    * 0개 이상의 반복을 위한 조합기
    */
  def many[A](p: Parser[A]): Parser[List[A]] = map2(p, many(p))(_ :: _) or succeed(List())

  /**
    * 연습문제 9.7
    */
  def product[A, B](p: Parser[A], p2: => Parser[B]): Parser[(A, B)] =
    // for (a <- p; b <- p2) yield ((a, b))
    p.flatMap { a =>
      p2.map { b => (a, b) }
    }

  /**
    * 연습문제 9.1
    * 연습문제 9.7
    */
  def map2[A,B,C](p: Parser[A], p2: => Parser[B])(f: (A, B) => C): Parser[C] =
    for (a <- p; b <- p2) yield f(a, b)
//    (p ** p2) map (f.tupled) /* equals to {case (a, b) => f(a, b)}*/
//    p.flatMap { a =>
//      p2.map { b => f(a, b) }
//    }

  // 1개 이상의 반복(non-empty repetition)을 위한 조합기
  def many1[A](p: Parser[A]): Parser[List[A]] = map2(p, many(p))(_ :: _)

  /**
    * 연습문제 9.8
    */
  // map(p)(a => a) == p
  def map[A, B](p: Parser[A])(f: A => B): Parser[B] = p.flatMap(a => succeed(f(a)))

  // 입력 문자열과 무관하게 항상 파싱에 성공해서 값 a를 돌려주는 파서
  // run(succeed(a))(s) == Right(a)
  def succeed[A](a: A): Parser[A] = string("").map(_ => a)

  def flatMap[A,B](p: Parser[A])(f: A => Parser[B]): Parser[B] = loc => {
    p(loc) match {
      case Success(a, n) => f(a)(loc.advanceBy(n))
                                .addCommit(n != 0)
                                .advanceSuccess(n)
      case f @ Failure(_, _) => f
    }
  }
//    p(loc) match {
//      case Success(a, consumed) => f(a)(loc)
//      case f @ Failure(_, _) => f
//    }

  def digit: Parser[String] = "[0-9]".r

  /**
    * 연습문제 9.6
    */
  def leadingDigitSensitive: Parser[List[Char]] =
    digit
      .map(_.toInt)
      .flatMap { (n: Int) => listOfN(n, char('a')) }

  // 0개 이상의 문자 'a'를 인식해서 그 개수를 돌려주는 파서
  // map(many(char('a')))(_.size)
  def numA: Parser[Int] = char('a').many.slice.map(_.size) // char('a').many.map(_.size)

  // p가 실패하면 ParserError가 msg를 제시하게 하는 오류 메시지 배정 조합기
  def label[A](msg: String)(p: Parser[A]): Parser[A] = loc => p(loc).mapError(_.label(msg))

  // p가 실패하면 msg를 덧붙이는 조합기
  def scope[A](msg: String)(p: Parser[A]): Parser[A] = loc => p(loc).mapError(_.push(loc, msg))

  ////////// ParserOps //////////
  case class ParserOps[A](p: Parser[A]) {
    def |[B>:A](p2: => Parser[B]): Parser[B] = self.or(p, p2)
    def or[B>:A](p2: => Parser[B]): Parser[B] = self.or(p, p2)

    def many: Parser[List[A]] = self.many(p)

    def slice: Parser[String] = self.slice(p)

    def map[B](f: A => B): Parser[B] = self.map(p)(f)

    def flatMap[B](f: A => Parser[B]): Parser[B] = self.flatMap(p)(f)

    def product[B](p2: Parser[B]): Parser[(A, B)] = self.product(p, p2)
    def **[B](p2: Parser[B]): Parser[(A, B)] = self.product(p, p2)
  }

  ////////// Laws //////////
  object Laws {
    def equal[A](p1: Parser[A], p2: Parser[A])(in: Gen[String]): Prop =
      Prop.forAll(in)(s => run(p1)(s) == run(p2)(s))

    def mapLaw[A](p: Parser[A])(in: Gen[String]): Prop = equal(p, p.map(a => a))(in)
  }
}

/* -------------------- Results -------------------- */
trait Result[+A] {
  def advanceSuccess(n: Int): Result[A] = this match {
    case Success(a, c) => Success(a, c + n)
    case _ => this
  }

  def addCommit(b: Boolean): Result[A] = this match {
    case Failure(e, false) if (b) => Failure(e, true)
    case _ => this
  }

  def mapError(f: ParseError => ParseError): Result[A] = this match {
    case Failure(e, c) => Failure(f(e), c)
    case _ => this
  }

  def uncommit: Result[A] = this match {
    case Failure(e, true) => Failure(e, false)
    case _ => this
  }
}

case class Success[+A](get: A, charsConsumed: Int) extends Result[A]
case class Failure(get: ParseError, isCommitted: Boolean) extends Result[Nothing]

/* -------------------- Errors -------------------- */
case class ParseError(stack: List[(Location, String)]) {
  def push(loc: Location, msg: String): ParseError = copy(stack = (loc, msg) :: stack)

  def label(msg: String): ParseError = {
    val latestLoc: Option[Location] = stack.lastOption.map(_._1)
    ParseError(latestLoc.map((_, msg)).toList)
  }

}

case class Location(input: String, offset: Int = 0) {
  lazy val line: Int = input.slice(0, offset + 1).count(_ == '\n') + 1

  lazy val col: Int = input.slice(0, offset + 1).lastIndexOf('\n') match {
    case -1 => offset + 1
    case lineStart => offset - lineStart
  }

  def toError(msg: String): ParseError = ParseError(List((this, msg)))

  def advanceBy(n: Int): Location = copy(offset = offset + n)
}
