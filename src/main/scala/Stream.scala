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
//  @annotation.tailrec
//  def take(n:Int):Stream[A]={
//    n match {
//      case 0 => Empty
//      case _=> conss(this.headOption, )
//    }
//  }
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
