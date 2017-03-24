package ch_3

object Ex3_11 extends App {

  def foldLeft[A, B](as: List[A], z: B)(f: (B, A) => B): B = as match {
    case Nil => z
    case Cons(x, xs) => foldLeft(xs, f(z, x))(f)
  }

  def sum(l: List[Int]): Int = foldLeft(l, 0)(_ + _)

  def product(l: List[Double]): Double = foldLeft(l, 1.0)(_ * _)

  println(sum(List(1, 2, 3, 4, 5)))
  println(product(List(1, 2, 3, 4, 5)))

}
