package gettingstarted02

/**
  * Created by honey.and.sw on 2017. 2. 16.
  */
object uncurry_2_4 {
  def uncurry[A, B, C](f: A => (B => C)): (A, B) => C = {
    (a, b) => f(a)(b)
  }
}
