package state06

trait RNG {
  def nextInt: (Int, RNG)
}

object RNG {

  case class SimpleRNG(seed: Long) extends RNG {
    override def nextInt: (Int, RNG) = {
      val newSeed = (seed * 0x5DEECD66DL + 0xBL) & 0xFFFFFFFFFFFFL
      val newRNG = SimpleRNG(newSeed)
      val n = (newSeed >>> 16).toInt
      (n, newRNG)
    }
  }

  /**
    * 연습문제 6.1
    * 0 이상, Int.MaxValue 이하의 난수 정수를 생성하는 함수
    */
  def nonNegativeInt(rng: RNG): (Int, RNG) = {
    val (i, r) = rng.nextInt

    if (i < 0 || i >= Int.MaxValue) (-(i + 1), r)
    else (i, r)
  }

  /**
    * 연습문제 6.2
    * 0 이상, 1 미만의 Double 난수를 발생하는 함수
    */
  def double(rng: RNG): (Double, RNG) = {
    val (i, r) = nonNegativeInt(rng)
    (i / (Int.MaxValue.toDouble + 1), r) // (i.toDouble / (Int.MaxValue + 1), r)
  }

  /**
    * 연습문제 6.5
    */
  def doubleWithMap(rng: RNG): (Double, RNG) = map[Int, Double](nonNegativeInt)(_ / (Int.MaxValue.toDouble + 1))(rng)

  /**
    * 연습문제 6.3
    */
  def intDouble(rng: RNG): ((Int, Double), RNG) = {
    val (i, r1) = rng.nextInt
    val (d, r2) = double(r1)
    ((i, d), r2)
  }

  def randIntDouble: Rand[(Int, Double)] = both(int, double)

  def doubleInt(rng: RNG): ((Double, Int), RNG) = {
    val ((i, d), r) = intDouble(rng)
    ((d, i), r)
  }

  def randDoubleInt: Rand[(Double, Int)] = both(double, int)

  def double3(rng: RNG): ((Double, Double, Double), RNG) = {
    val (d1, r1) = double(rng)
    val (d2, r2) = double(r1)
    val (d3, r3) = double(r2)
    ((d1, d2, d3), r3)
  }

  /**
    * 연습문제 6.4
    */
  def ints(count: Int)(rng: RNG): (List[Int], RNG) = {
    if (count <= 0) (Nil, rng)
    else {
      val (i, r1) = rng.nextInt
      val (list, r2) = ints(count - 1)(r1)
      (i +: list, r2)
    }
  }

  def ints2(count: Int)(rng: RNG): (List[Int], RNG) = {
    def go(count: Int, list: List[Int])(rng: RNG): (List[Int], RNG) = {
      if (count <= 0) (list, rng)
      else {
        val (i, r) = rng.nextInt
        go(count - 1, i +: list)(r)
      }
    }

    go(count, List())(rng)
  }

  /**
    * 연습문제 6.7
    */
  def intsWithSequence(count: Int)(rng: RNG): (List[Int], RNG) =
    sequence(List.fill[Rand[Int]](count)(int))(rng)

  /* --------------- 상태 동작을 위한 더 나은 API --------------- */

  type Rand[+A] = RNG => (A, RNG)

  def int: Rand[Int] = _.nextInt

  /**
    * 주어진 RNG를 사용하지 않고 그대로 전달하는 상태 전이 동작
    */
  def unit[A](a: A): Rand[A] = (a, _)

  def map[A, B](s: Rand[A])(f: A => B): Rand[B] = { rng =>
    val (a, rng2): (A, RNG) = s(rng)
    (f(a), rng2)
  }

  def nonNegativeEven: Rand[Int] = map(nonNegativeInt)(n => n - n % 2)

  /**
    * 연습문제 6.6
    */
  def map2[A, B, C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] = rng => {
    val (a, raa) = ra(rng)
    val (b, rbb) = rb(raa)
    (f(a, b), rbb)
  }

  def both[A, B](ra: Rand[A], rb: Rand[B]): Rand[(A, B)] = map2(ra, rb)((_, _))

  /**
    * 연습문제 6.7
    */
  def sequence[A](fs: List[Rand[A]]): Rand[List[A]] =
    fs.foldRight[Rand[List[A]]](unit(List())){ (f: Rand[A], acc: Rand[List[A]]) =>
      map2(f, acc)(_ +: _)
    }

  def _sequence[A](fs: List[Rand[A]]): Rand[List[A]] = rng => {
    if (fs.isEmpty) (List(), rng)
    else {
      val (l, r1): (List[A], RNG) = _sequence(fs.tail)(rng)
      val (a, r2) = fs.head(r1)
      (a +: l, r2)
    }
  }

  /**
    * 연습문제 6.8
    */
  def flatMap[A, B](f: Rand[A])(g: A => Rand[B]): Rand[B] = rng => {
    val (a, r1): (A, RNG) = f(rng)
    g(a)(r1)
  }

  def nonNegativeLessThan(n: Int): Rand[Int] = flatMap(int) { i =>
    val mod = i % n
    if (i + (n - 1) - mod >= 0) unit(mod) else nonNegativeLessThan(n)
  }

  /**
    * 연습문제 6.9
    */
  def mapWithFlatMap[A, B](s: Rand[A])(f: A => B): Rand[B] =
    flatMap[A, B](s)(a => unit(f(a)))

  def map2WithFlatMap[A, B, C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] =
    flatMap(ra){ a =>
      map(rb)(b => f(a, b))
    }
  // flatMap[(A, B), C](both(ra, rb))(ab => unit(f(ab._1, ab._2)))
}
