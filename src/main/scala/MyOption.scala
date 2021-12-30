import scala.runtime.Nothing$

sealed trait MyOption[+A] {
  def map[B](f: A => B): MyOption[B] = {
    this match {
      case Some(x) => Some(f(x))
      case _ => None
    }
  }

  def getOrElse[B >: A](default: => B): B={
    this match {
      case Some(x)=>x
      case _=> default
    }
  }
  def flatMap[B](f: A => MyOption[B]): MyOption[B]={
    map(f).getOrElse(None)
  }
  def orElse[B >: A](ob: => MyOption[B]): MyOption[B]={
    this.map(Some(_)).getOrElse(ob)
  }
  def filter(f: A => Boolean): MyOption[A]={
    this flatMap(x=>if (f(x)) Some(x) else None:MyOption[A])
  }


}

case class Some[+A](get:A) extends MyOption[A]
case object None extends MyOption[Nothing]
object MyOption {
  def map2[A,B,C](a: MyOption[A], b: MyOption[B])(f: (A, B) => C): MyOption[C]={
    a.flatMap(x=> b.map(y=>f(x,y)))
  }
  def sequence[A](a: MyList[MyOption[A]]): MyOption[MyList[A]]={
    MyList.foldLeft(a,Some(Nil):MyOption[MyList[A]])((acc, el)=>map2(acc,el)(MyList.appendLeft))
  }
  def traverse[A, B](a: MyList[A])(f: A => MyOption[B]): MyOption[MyList[B]]={
    a match {
      case Nil => Some(Nil)
      case Cons(h,t)=> map2(f(h), traverse(t)(f))(Cons(_,_))
    }
  }

}
