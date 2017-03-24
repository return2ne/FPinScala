package ch_3

object Ex3_4 extends App {

  def tail[A](as: List[A]): List[A] = as match {
    case Nil => Nil
    case Cons(_, xs) => xs
  }

  def drop[A](l: List[A], n: Int): List[A] = {
    @annotation.tailrec
    def loop(num: Int, ll: List[A]): List[A] = {
      if (num == 0) ll
      else loop(num - 1, tail(ll))
    }

    loop(n, l)
  }

  println(drop(List(1, 2, 3, 4, 5), 2))

}
