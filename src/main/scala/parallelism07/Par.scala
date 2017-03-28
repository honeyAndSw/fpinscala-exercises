package parallelism07

import java.util.concurrent.{Callable, TimeUnit, Future, ExecutorService}

object Par {

  type Par[A] = ExecutorService => Future[A]

  /**
    * Future를 아주 간단하게 구현한 상수 get의 Wrapper
    */
  private case class UnitFuture[A](get: A) extends Future[A] {
    override def isCancelled: Boolean = false
    override def get(timeout: Long, unit: TimeUnit): A = get
    override def cancel(mayInterruptIfRunning: Boolean): Boolean = false
    override def isDone: Boolean = true
  }

  def unit[A](a: A): Par[A] = (es: ExecutorService) => UnitFuture(a)

  def map[A, B](a: Par[A])(f: A => B): Par[B] = map2(a, unit(()))((a, _) => f(a))

  /**
    * map을 이용해서 리스트를 정렬하는 함수
    */
  def sortPar(parList: Par[List[Int]]) = map(parList)(_.sorted)

  def map2[A, B, C](a: Par[A], b: Par[B])(f: (A, B) => C): Par[C] = (es: ExecutorService) => {
    val af: Future[A] = a(es)
    val bf: Future[B] = b(es)
    UnitFuture(f(af.get, bf.get))
  }

  def fork[A](a: => Par[A]): Par[A] = (es: ExecutorService) => {
    es.submit(new Callable[A] {
      override def call(): A = a(es).get
    })
  }

  /**
   * 계산의 인스턴스화를 실제로 필요한 시점까지 미루는 용도의 함수
   */
  def delay[A](a: => Par[A]): Par[A] = (es: ExecutorService) => a(es)

  def lazyUnit[A](a: => A): Par[A] = fork(unit(a))

  def run[A](es: ExecutorService)(a: Par[A]): Future[A] = a(es)

  /**
    * 연습문제 7.4
    */
  def asyncF[A, B](f: A => B): A => Par[B] = (a: A) => lazyUnit(f(a))

  /**
    * 연습문제 7.5
    */
  def sequence[A](ps: List[Par[A]]): Par[List[A]] =
    ps.foldRight(unit(List[A]())){ (p: Par[A], acc: Par[List[A]]) =>
      map2(p, acc)((p1, acc1) => p1 +: acc1)
    }

  def parMap[A, B](as: List[A])(f: A => B): Par[List[B]] = fork {
    val fps: List[Par[B]] = as.map(asyncF(f))
    sequence(fps)
  }

  /**
    * 연습문제 7.6
    */
  def parFilter[A](as: List[A])(f: A => Boolean): Par[List[A]] = {
    val fas: List[Par[List[A]]] = as.map(asyncF{ a =>
      if (f(a)) List(a) else List()
    })

    val seq: Par[List[List[A]]] = sequence(fas)
    map(seq)(_.flatten)
  }

  def choice[A](cond: Par[Boolean])(t: Par[A], f: Par[A]): Par[A] = es => {
    if (run(es)(cond).get) t(es)
    else f(es)
  }

  /**
    * 연습문제 7.11
    */
  def choiceN[A](n: Par[Int])(choices: List[Par[A]]): Par[A] = es => {
    val idx = run(es)(n).get
    choices(idx)(es) // solution : run(es)(choices(idx))
  }

  def choiceViaChoiceN[A](cond: Par[Boolean])(t: Par[A], f: Par[A]): Par[A] =
    choiceN(map(cond)(b => if (b) 0 else 1))(List(t, f))

  /**
    * 연습문제 7.13
    */
  def chooser[A, B](pa: Par[A])(choices: A => Par[B]): Par[B] = es => {
    val a: A = run(es)(pa).get()
    run(es)(choices(a))
  }

  def choiceViaChooser[A](cond: Par[Boolean])(t: Par[A], f: Par[A]): Par[A] =
    chooser(cond)(b => if (b) t else f)

  def choiceNViaChooser[A](n: Par[Int])(choices: List[Par[A]]): Par[A] =
    chooser(n)(i => choices(i))


}
