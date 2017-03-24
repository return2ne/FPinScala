package ch_3

object Ex3_5 extends App {

  def dropWhile[A](l: List[A], f: A => Boolean): List[A] = l match {
    case Cons(x, xs) if (f(x)) => dropWhile(xs, f)
    case _ => l
  }

  println(dropWhile(List(1, 2, 3, 4, 5), (x: Int) => x % 2 == 0))

}
