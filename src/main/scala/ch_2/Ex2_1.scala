package ch_2

object Ex2_1 {

  def fib(n: Int): Int = {
    @annotation.tailrec
    def loop(n: Int, prev: Int, cur: Int): Int =
      if (n == 0) prev
      else loop(n - 1, cur, prev + cur)

    loop(n, 0, 1)
  }

  def main(args: Array[String]): Unit = {
    println(fib(5))
    // 0, 1, 1, 2, 3, 5
  }

}
