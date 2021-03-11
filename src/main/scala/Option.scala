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

