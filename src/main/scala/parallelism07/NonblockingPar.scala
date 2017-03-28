package parallelism07

import java.util.concurrent.{Callable, CountDownLatch, ExecutorService}
import java.util.concurrent.atomic.AtomicReference

/**
  * Created by honey.and.sw on 2017. 3. 28.
  */
object NonblockingPar {
  sealed trait Future[A] {
    private[parallelism07] def apply(cb: A => Unit): Unit
  }

  type Par[A] = ExecutorService => Future[A]

  def run[A](es: ExecutorService)(p: Par[A]): A = {
    val ref = new AtomicReference[A]
    val latch = new CountDownLatch(1)

    p(es) { a: A =>
      ref.set(a)
      latch.countDown()
    }

    latch.await() // countDown이 1번 호출될 때 까지 대기
    ref.get()
  }

  def unit[A](a: A): Par[A] = es => new Future[A] {
    override def apply(cb: (A) => Unit): Unit = cb(a)
  }

  def fork[A](a: => Par[A]): Par[A] = es => new Future[A] {
    override def apply(cb: (A) => Unit): Unit = eval(es)(a(es)(cb))
  }

  def eval(es: ExecutorService)(r: => Unit): Unit = es.submit(new Callable[Unit] {
    override def call(): Unit = r
  })

}
