import scala.runtime.Nothing$

sealed trait Option[+A] {
  def map[B](f: A => B): Option[B] = {
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
  def flatMap[B](f: A => Option[B]): Option[B]={
    map(f).getOrElse(None)
  }
  def orElse[B >: A](ob: => Option[B]): Option[B]={
    this.map(Some(_)).getOrElse(ob)
  }
  def filter(f: A => Boolean): Option[A]={
    this flatMap(x=>if (f(x)) Some(x) else None:Option[A])
  }


}

case class Some[+A](get:A) extends Option[A]
case object None extends Option[Nothing]
object Option {
  def map2[A,B,C](a: Option[A], b: Option[B])(f: (A, B) => C): Option[C]={
    a.flatMap(x=> b.map(y=>f(x,y)))
  }
  def sequence[A](a: List[Option[A]]): Option[List[A]]={
    List.foldLeft(a,Some(Nil):Option[List[A]])((acc, el)=>map2(acc,el)(List.appendLeft))
  }
  def traverse[A, B](a: List[A])(f: A => Option[B]): Option[List[B]]={
    a match {
      case Nil => Some(Nil)
      case Cons(h,t)=> map2(f(h), traverse(t)(f))(Cons(_,_))
    }
  }

}
