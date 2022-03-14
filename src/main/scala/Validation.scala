import Applicative.Applicative

sealed trait Validation[+E, +A]
case class Failure[E](head: E, tail: Vector[E] = Vector())
    extends Validation[E, Nothing]
case class Success[A](a: A) extends Validation[Nothing, A]

object appValidation extends Applicative[({type f[x] = Validation[String, x]})#f] {
    override def map2[A, B, C](fa: Validation[String, A], fb: Validation[String, B])(f: (A, B) => C): Validation[String, C] =
        (fa,fb) match {
            case (a @ Failure(_,_), b @ Failure(_,_))=>Failure[String](a.head, a.tail ++ (b.head +: b.tail))
            case (_, b @ Failure(_,_))=>b
            case (b @ Failure(_,_),_)=>b
            case (Success(a), Success(b)) => Success(f(a,b))
        }

    override def unit[A](a: => A): Validation[String, A] = Success(a)
}