package ch_3

object Ex3_3 extends App {

  def setHead[A](x: A, as: List[A]): List[A] = as match {
    case Nil => Nil
    case Cons(_, xs) => Cons(x, xs)
  }

  println(setHead(4, List(1, 2, 3, 4, 5)))

}
