object object1 extends App{
  def sum(ints: List[Int]): Int = ints match {
    case Nil => 0
    case Cons(x,xs) => x + sum(xs)
  }
  def product(ds: List[Double]): Double = ds match {
    case Nil => 1.0
    case Cons(0.0, _) => 0.0
    case Cons(x,xs) => x * product(xs)
  }

  val x = List(1,2,3,4,5) match {
    case Cons(x, Cons(2, Cons(4, _))) => x
    case Nil => 42
    case Cons(x, Cons(y, Cons(3, Cons(4, _)))) => x + y
    case Cons(h, t) => h + sum(t)
    case _ => 101
  }
  def init(l1:List[Int]):List[Int] = {
    l1 match {
      case Nil => Nil
      case Cons(h,t) => t match {
        case Nil => Nil
        case _ => Cons(h, init(t))
      }
    }
  }

  //  println(List.foldLeft(List(1,2,3,4), Nil:List[Int])((x,y)=> Cons(y,x)))
  //  println(List.foldRight(List(1,2,3,4), Nil)((x,y)=> 1+y))
  println(List.hasSubSeq(List(1,2,3), List(2,3)))
}

