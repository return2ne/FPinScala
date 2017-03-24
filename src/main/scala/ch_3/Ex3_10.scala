package ch_3

object Ex3_10 extends App {

  @annotation.tailrec
  def foldLeft[A, B](as: List[A], z: B)(f: (B, A) => B): B = as match {
    case Nil => z
    case Cons(x, xs) => foldLeft(xs, f(z, x))(f)
  }

  println(foldLeft(List(1, 2, 3, 4), 0)((_, y) => y + 1))
}
