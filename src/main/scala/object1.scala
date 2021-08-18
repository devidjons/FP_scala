object object1 extends App{
  def sum(ints: MyList[Int]): Int = ints match {
    case Nil => 0
    case Cons(x,xs) => x + sum(xs)
  }
  def product(ds: MyList[Double]): Double = ds match {
    case Nil => 1.0
    case Cons(0.0, _) => 0.0
    case Cons(x,xs) => x * product(xs)
  }

  val x = MyList(1,2,3,4,5) match {
    case Cons(x, Cons(2, Cons(4, _))) => x
    case Nil => 42
    case Cons(x, Cons(y, Cons(3, Cons(4, _)))) => x + y
    case Cons(h, t) => h + sum(t)
    case _ => 101
  }
  def init(l1:MyList[Int]):MyList[Int] = {
    l1 match {
      case Nil => Nil
      case Cons(h,t) => t match {
        case Nil => Nil
        case _ => Cons(h, init(t))
      }
    }
  }
  val ones: MyStream[Int] = MyStream.conss(1, ones)
  val r1 =MyStream(1,2,3,4,5,6).scanRight(0)(_ + _).toList
  println(r1)

  //  println(List.foldLeft(List(1,2,3,4), Nil:List[Int])((x,y)=> Cons(y,x)))
  //  println(List.foldRight(List(1,2,3,4), Nil)((x,y)=> 1+y))
//  println(List.hasSubSeq(List(1,2,3,4,5,6,7), List(2,3)))
}

object test1 extends App{
  println(Machine.simulateMachine(List(Coin, Turn, Coin, Turn, Turn))(Machine(true, 5, 5))._2)
}

