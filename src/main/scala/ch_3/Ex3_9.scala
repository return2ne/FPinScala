package ch_3

object Ex3_9 extends App {

  def foldRight[A, B](as: List[A], z: B)(f: (A, B) => B): B = as match {
    case Nil => z
    case Cons(x, xs) => f(x, foldRight(xs, z)(f))
  }

  def length[A](as: List[A]): Int = foldRight(as, 0)((_, y) => y + 1)

  println(length(List(1, 2, 3, 7, 6)))

}
