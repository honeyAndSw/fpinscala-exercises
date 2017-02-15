package gettingstarted02

/**
  * 2.2 isSorted
  */
object issorted_2_2 {
  def isSorted[A](as: Array[A], ordered: (A, A) => Boolean): Boolean = {
    as.length match {
      case l if l < 2 => false
      case _ => {
        if (ordered(as.head, as.tail.head)) isSorted(as.tail, ordered)
        else false
      }
    }
  }
}
