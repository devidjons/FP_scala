import MyStream.{conss, unfold}

import scala.::
import scala.collection.immutable.List._

trait MyStream[+A] {
//  def headOption: Option[A] = this match {
//    case Empty => None
//    case Conss(h, t) => Some(h())
//  }

  def toList: MyList[A] = {
    @annotation.tailrec
    def go[A](s: MyStream[A], acc: MyList[A]): MyList[A] = {
      s match {
        case Empty => acc
        case Conss(h, t) => go(t(), Cons(h(), acc))
      }

    }

    MyList.reverse(go(this, Nil))
  }
  def take(n:Int):MyStream[A]={
    n match {
      case 0 => Empty:MyStream[A]
      case _=> {
        this match {
          case Empty => Empty:MyStream[A]
          case Conss(h,t)=> conss(h(), t().take(n-1))
        }
      }
    }
  }
  def drop(n:Int):MyStream[A]={
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
  def find(p:A=> Boolean):MyOption[A] = {
    this.filter(p) match {
      case Empty => None
      case Conss(h,t) => Some(h())
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
  def takeWhile(p: A => Boolean): MyStream[A] = {
    this.foldRight(Empty:MyStream[A])((el, acc)=> if (!p(el)) Empty else conss(el,acc) )
  }
  def headOption: MyOption[A] ={
    this.foldRight(None:MyOption[A])((el, acc) => Some(el))
  }
  def map[B](f:A=>B):MyStream[B] = {
    this.foldRight(Empty:MyStream[B])((el, acc)=> conss(f(el),acc))
  }
  def map2[B](f:A=>B):MyStream[B] = {
    MyStream.unfold(this) {
      case Empty => None
      case Conss(h, t) => Some(f(h()), t())
    }
  }
  def take2(n:Int):MyStream[A]={
    unfold((this,n)){
      case (_,0)=> None
      case (Empty,_)=> None
      case (Conss(h,t),n1)=>Some((h(),(t(),n1-1)))
    }
  }
  def takeWhile2(p: A => Boolean): MyStream[A] = {
    unfold(this){
      case Conss(h,t) if p(h())=> Some((h(), t()))
      case _ => None
    }
  }
  def filter(f:A=>Boolean):MyStream[A]={
    this.foldRight(Empty:MyStream[A])((el, acc)=> if (f(el)) conss(el, acc) else acc)
  }
  def append[B>:A](s: => MyStream[B]): MyStream[B] =
    foldRight(s)((el,acc)=>conss(el,acc))

  def flatMap[B](f: A => MyStream[B]): MyStream[B] ={
    foldRight(MyStream.empty[B])((el, acc)=>f(el).append(acc))
  }
  def zipWith[B,C](b: MyStream[B])(f: (A,B) => C):MyStream[C]={
    unfold((this,b)){
      case (Conss(h1,t1), Conss(h2,t2)) =>Some((f(h1(),h2()), (t1(), t2() )))
      case _ => None
    }
  }
  def zipAll[B](s2: MyStream[B]): MyStream[(MyOption[A],MyOption[B])]={
    unfold((this, s2)){
      case (Empty, Empty)=> None
      case (Empty, Conss(h,t))=> {
        val r1 =(
          (None:MyOption[A], Some(h())),
          (MyStream.empty[A], t())
        )
        Some(r1)
      }
      case (Conss(h,t), Empty) => {
        val r1 =(
          (Some(h()),None:MyOption[B]),
          (t(),MyStream.empty[B])
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
  def startsWith[A](s: MyStream[A]): Boolean={
    (this, s) match {
      case (_, Empty)=> true
      case (Conss(h1, t1), Conss(h2,t2))=> h1()==h2() && t1().startsWith(t2())
    }
  }
  def tails: MyStream[MyStream[A]]={
    unfold(this)({
      case Conss(h,t)=> Some(Conss(h,t), t())
      case Empty => None}).append(MyStream(MyStream.empty))
  }
  def scanRight[B](z:B)(f:(A,B)=>B):  Conss[B]={
    println("calculation")
    this match {
      case Empty => Conss(()=>z, ()=>Empty:MyStream[B])
      case Conss(h,t)=> {
        lazy val res = t().scanRight(z)(f)
        Conss(() => f(h(), res.h()),  ()=> res)
      }
    }
  }


}
case object Empty extends MyStream[Nothing]
case class Conss[+A](h: () => A, t: () => MyStream[A]) extends MyStream[A]
object MyStream {
  def conss[A](hd: => A, tl: => MyStream[A]): MyStream[A] = {
    val head = hd
    lazy val tail = tl
    Conss(() => head, () => tail)
  }
  def empty[A]: MyStream[A] = Empty
  def apply[A](as: A*): MyStream[A] =
    if (as.isEmpty) empty else conss(as.head, apply(as.tail: _*))
  def constant[A](a: A): MyStream[A]={
    MyStream.conss(a, constant(a))
  }
  def from(n: Int): MyStream[Int]={
    MyStream.conss(n, from(n+1))
  }
  def fibs(a0: Int=0, a1:Int=1):MyStream[Int]={
    conss(a0, fibs(a1, a1+a0))
  }
  def unfold[A, S](z: S)(f: S => MyOption[(A, S)]): MyStream[A]={
      f(z) match {
        case None => Empty
        case Some(x)=> conss(x._1,unfold(x._2)(f))
      }
    }
  def fibs2():MyStream[Int]={
    unfold((0,1))(x=>Some(x._1, (x._2, x._1+x._2)))
  }

//  Use unfold to implement map, take, takeWhile, zipWith

}
