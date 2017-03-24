package ch_3

object Ex3_2 extends App {

  def tail[A](as: List[A]): List[A] = as match {
    case Nil => Nil
    case Cons(_, xs) => xs
  }

  println(tail(List(1, 2, 3, 4, 5)))

}
