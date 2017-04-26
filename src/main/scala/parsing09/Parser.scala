package parsing09

import testing08._

trait Parsers[ParserError, Parser[+_]] { self =>
  def run[A](p: Parser[A])(input: String): Either[ParserError, A]

  // run(string(s))(s) == Right(s)
  implicit def string(s: String): Parser[String]
  implicit def asStringParser[A](a: A)(implicit f: A => Parser[String]): ParserOps[String] = ParserOps(f(a))
  implicit def operators[A](p: Parser[A]) = ParserOps[A](p)

  // run(char(c))(c.toString) == Right(c)
  def char(c: Char): Parser[Char] = string(c.toString) map (_.charAt(0))

  // run(or(string("abra"), string("cadabra")))("abra") == Right("abra")
  // run(or(string("abra"), string("cadabra")))("cadabra") == Right("cadabra")
  def or[A](s1: Parser[A], s2: Parser[A]): Parser[A]

  def listOfN[A](n: Int, p: Parser[A]): Parser[List[A]]

  // 입력 문자열 중 파서가 조사한 조각만 돌려주는 파서
  // run(slice(('a'|'b').many))("aaba") == Right("aaba")
  def slice[A](p: Parser[A]): Parser[String]

  def many[A](p: Parser[A]): Parser[List[A]]

  // map(p)(a => a) == p
  def map[A, B](p: Parser[A])(f: A => B): Parser[B]

  // 0개 이상의 문자 'a'를 인식해서 그 개수를 돌려주는 파서
  // map(many(char('a')))(_.size)
  def numA: Parser[Int] = char('a').many.map(_.size)

  // 입력 문자열과 무관하게 항상 파싱에 성공해서 값 a를 돌려주는 파서
  // run(succeed(a))(s) == Right(a)
  def succeed[A](a: A): Parser[A] = string("").map(_ => a)

  /* -------------------- ParserOps -------------------- */
  case class ParserOps[A](p: Parser[A]) {
    def |[B>:A](p2: Parser[B]): Parser[B] = self.or(p, p2)
    def or[B>:A](p2: Parser[B]): Parser[B] = self.or(p, p2)

    def many: Parser[List[A]] = self.many(p)

    def map[B](f: A => B): Parser[B] = self.map(p)(f)
  }

  /* -------------------- Laws -------------------- */
  object Laws {
    def equal[A](p1: Parser[A], p2: Parser[A])(in: Gen[String]): Prop =
      Prop.forAll(in)(s => run(p1)(s) == run(p2)(s))

    def mapLaw[A](p: Parser[A])(in: Gen[String]): Prop = equal(p, p.map(a => a))(in)
  }
}

