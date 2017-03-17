package ch_2

object Ex2_2 {

  def isSorted[A](as: Array[A], ordered: (A, A) => Boolean): Boolean = {
    @annotation.tailrec
    def loop(n: Int): Boolean = {
      if (n >= as.length - 1) true
      else if (ordered(as(n), as(n + 1))) false
      else loop(n + 1)
    }

    loop(0)
  }

  def order(x: Int, y: Int): Boolean = {
    x > y
  }

  def main(args: Array[String]): Unit = {
    println(isSorted(Array(1, 4, 3, 4, 5), order))
    println(isSorted(Array(1, 2, 3, 4, 5), (x: Int, y: Int) => x > y))
  }
}
