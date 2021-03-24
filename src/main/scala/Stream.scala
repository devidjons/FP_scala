import Stream.{conss, unfold}

import scala.::
import scala.collection.immutable.List._

trait Stream[+A] {
//  def headOption: Option[A] = this match {
//    case Empty => None
//    case Conss(h, t) => Some(h())
//  }

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
//  def takeWhile(p: A => Boolean): Stream[A] = {
//    this match {
//      case Conss(h,t) if p(h())=> conss(h(), t().takeWhile(p))
//      case _ => Stream.empty
//    }
//  }
  def exists(p: A => Boolean): Boolean = {
    this match {
      case Conss(h, t) => p(h()) || t().exists(p)
      case _ => false
    }
  }
  def foldRight[B](z: => B)(f: (A, => B) => B): B = {

    this match {
      case Conss(h,t) => f(h(), t().foldRight(z)(f))
      case _ => z
    }
  }
  def forAll(p: A => Boolean): Boolean={
    this.foldRight(true)((el, acc)=> p(el) && acc)
  }
  def takeWhile(p: A => Boolean): Stream[A] = {
    this.foldRight(Empty:Stream[A])((el, acc)=> if (!p(el)) Empty else conss(el,acc) )
  }
  def headOption: Option[A] ={
    this.foldRight(None:Option[A])((el, acc) => Some(el))
  }
  def map[B](f:A=>B):Stream[B] = {
    this.foldRight(Empty:Stream[B])((el, acc)=> conss(f(el),acc))
  }
  def map2[B](f:A=>B):Stream[B] = {
    Stream.unfold(this) {
      case Empty => None
      case Conss(h, t) => Some(f(h()), t())
    }
  }
  def take2(n:Int):Stream[A]={
    unfold((this,n)){
      case (_,0)=> None
      case (Empty,_)=> None
      case (Conss(h,t),n1)=>Some((h(),(t(),n1-1)))
    }
  }
  def takeWhile2(p: A => Boolean): Stream[A] = {
    unfold(this){
      case Conss(h,t) if p(h())=> Some((h(), t()))
      case _ => None
    }
  }
  def filter(f:A=>Boolean):Stream[A]={
    this.foldRight(Empty:Stream[A])((el,acc)=> if (f(el)) conss(el, acc) else acc)
  }
  def append[B>:A](s: => Stream[B]): Stream[B] =
    foldRight(s)((el,acc)=>conss(el,acc))

  def flatMap[B](f: A => Stream[B]): Stream[B] ={
    foldRight(Stream.empty[B])((el,acc)=>f(el).append(acc))
  }
  def zipWith[B,C](b: Stream[B])(f: (A,B) => C):Stream[C]={
    unfold((this,b)){
      case (Conss(h1,t1), Conss(h2,t2)) =>Some((f(h1(),h2()), (t1(), t2() )))
      case _ => None
    }
  }
  def zipAll[B](s2: Stream[B]): Stream[(Option[A],Option[B])]={
    unfold((this, s2)){
      case (Empty, Empty)=> None
      case (Empty, Conss(h,t))=> {
        val r1 =(
          (None:Option[A], Some(h())),
          (Stream.empty[A], t())
        )
        Some(r1)
      }
      case (Conss(h,t), Empty) => {
        val r1 =(
          (Some(h()),None:Option[B]),
          (t(),Stream.empty[B])
        )
        Some(r1)
      }
      case (Conss(h1,t1), Conss(h2,t2))=>{
        val r1 =(
          (Some(h1()),Some(h2())),
          (t1(),t2())
        )
        Some(r1)
      }
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
  def constant[A](a: A): Stream[A]={
    Stream.conss(a, constant(a))
  }
  def from(n: Int): Stream[Int]={
    Stream.conss(n, from(n+1))
  }
  def fibs(a0: Int=0, a1:Int=1):Stream[Int]={
    conss(a0, fibs(a1, a1+a0))
  }
  def unfold[A, S](z: S)(f: S => Option[(A, S)]): Stream[A]={
      f(z) match {
        case None => Empty
        case Some(x)=> conss(x._1,unfold(x._2)(f))
      }
    }
  def fibs2():Stream[Int]={
    unfold((0,1))(x=>Some(x._1, (x._2, x._1+x._2)))
  }

//  Use unfold to implement map, take, takeWhile, zipWith

}
