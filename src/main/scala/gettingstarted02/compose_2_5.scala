package gettingstarted02

/**
  * Created by honey.and.sw on 2017. 2. 16.
  */
object compose_2_5 {
  def compose[A, B, C](f: B => C, g: A => B): A => C = {
    a => f(g(a))
  }
}
