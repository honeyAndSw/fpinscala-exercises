package localeffects14



/**
  * 실행하기에 안전한 ST 동작들, 즉 S에 대해 다형적인 동작들을 대표한다.
  */
trait RunnableST[A] {
  def apply[S]: ST[S, A]
}

sealed trait STRef[S, A] {
  protected var cell: A

  def read: ST[S,A] = ST(cell)
  def write(a: A): ST[S,Unit] = new ST[S,Unit] {
    def run(s: S): (Unit, S) = {
      cell = a
      ((), s)
    }
  }
}

object STRef {
  def apply[S,A](a: A): ST[S, STRef[S,A]] = ST(new STRef[S,A] {
    var cell: A = a
  })
}

sealed trait ST[S, A] { self =>
  // Protected Scope : http://alvinalexander.com/scala/how-to-control-scala-method-scope-object-private-package#protected-scope
  // sealed https://stackoverflow.com/questions/11203268/what-is-a-sealed-trait

  // S : 상태의 변이 능력
  protected def run(s: S): (A, S)

  def map[B](f: A => B): ST[S,B] = new ST[S,B] {
    override protected def run(s: S): (B, S) = {
      val (a, s1): (A, S) = self.run(s)
      (f(a), s1)
    }
  }

  def flatMap[B](f: A => ST[S,B]): ST[S,B] = new ST[S,B] {
    override protected def run(s: S): (B, S) = {
      val (a, s1): (A, S) = self.run(s)
      f(a).run(s1)
    }
  }
}

object ST {
  def apply[S,A](a: => A) = {
    lazy val memo = a
    new ST[S,A] {
      override protected def run(s: S): (A, S) = (memo, s)
    }
  }

  def runST[A](rst: RunnableST[A]): A = {
    val st: ST[Unit, A] = rst.apply[Unit]
    st.run(())._1
  }
}