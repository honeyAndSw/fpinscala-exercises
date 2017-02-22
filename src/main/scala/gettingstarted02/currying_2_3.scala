package gettingstarted02

/**
  * Created by honey.and.sw on 2017. 2. 16.
  */
object currying_2_3 {
  def curry[A, B, C](f: (A, B) => C): A => (B => C) = {
    (a: A) => { (b: B) => f(a, b) }
  }
}
