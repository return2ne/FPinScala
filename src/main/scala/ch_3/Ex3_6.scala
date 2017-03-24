package ch_3

object Ex3_6 extends App {

  def init[A](l: List[A]): List[A] = l match {
    case Cons(x, xs) if (xs != Nil) => Cons(x, init(xs))
    case _ => Nil
  }

  println(init(List(1, 2, 3, 4, 5)))

}
