sealed trait Either[+E, +A]{
  def map[B](f: A => B): Either[E, B]={
    this match {
      case Right(x)=>Right(f(x))
      case Left(x)=>Left(x)
    }
  }
  def flatMap[EE >: E, B](f: A => Either[EE, B]): Either[EE, B] = {
    this match {
      case Right(x) => f(x)
      case Left(x)=> Left(x)
    }
  }
  def orElse[EE >: E,B >: A](b: => Either[EE, B]): Either[EE, B]={
    this match {
      case Left(x) => Left(x)
      case Right(x) => b
    }
  }
  def map2[EE >: E, B, C](b: Either[EE, B])(f: (A, B) => C): Either[EE, C]={
    for{aa <- this; bb<-b } yield f(aa,bb)
  }

}
case class Left[+E](value: E) extends Either[E, Nothing]
case class Right[+A](value: A) extends Either[Nothing, A]

object Either{
  def sequence[E, A](es: List[Either[E, A]]): Either[E, List[A]]={
    es match {
      case Nil => Right(Nil)
      case Cons(h,t)=> h.map2(sequence(t))(Cons(_,_))
    }
  }
  def traverse[E, A, B](as: List[A])(f: A => Either[E, B]): Either[E, List[B]]={
    as match {
      case Nil => Right(Nil)
      case Cons(h,t)=>f(h).map2(traverse(t)(f))(Cons(_,_))
    }
  }

}
