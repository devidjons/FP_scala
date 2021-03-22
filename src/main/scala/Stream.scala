import Stream.conss

import scala.::
import scala.collection.immutable.List._

trait Stream[+A] {
  def headOption: Option[A] = this match {
    case Empty => None
    case Conss(h, t) => Some(h())
  }

  def toList: List[A] = {
    @annotation.tailrec
    def go[A](s: Stream[A], acc: List[A]): List[A] = {
      s match {
        case Empty => acc
        case Conss(h, t) => go(t(), Cons(h(), acc))
      }

    }

    List.reverse(go(this, List()))
  }
  def take(n:Int):Stream[A]={
    n match {
      case 0 => Empty:Stream[A]
      case _=> {
        this match {
          case Empty => Empty:Stream[A]
          case Conss(h,t)=> conss(h(), t().take(n-1))
        }
      }
    }
  }
  def drop(n:Int):Stream[A]={
    this match {
      case Conss(_,_) if n==0 => this
      case Conss(_,t) if n>0 => t().drop(n-1)
      case _ => Empty
    }
  }
  def takeWhile(p: A => Boolean): Stream[A] = {
    this match {
      case Conss(h,t) if p(h())=> conss(h(), t().takeWhile(p))
      case _ => Stream.empty
    }
  }
}
case object Empty extends Stream[Nothing]
case class Conss[+A](h: () => A, t: () => Stream[A]) extends Stream[A]
object Stream {
  def conss[A](hd: => A, tl: => Stream[A]): Stream[A] = {
    val head = hd
    lazy val tail = tl
    Conss(() => head, () => tail)
  }
  def empty[A]: Stream[A] = Empty
  def apply[A](as: A*): Stream[A] =
    if (as.isEmpty) empty else conss(as.head, apply(as.tail: _*))
}
