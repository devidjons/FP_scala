sealed trait MyList[+A]
case object Nil extends MyList[Nothing]
case class Cons[+A](head: A, tail: MyList[A]) extends MyList[A]
object MyList {
  def sum(ints: MyList[Int]): Int = ints match {
    case Nil => 0
    case Cons(x,xs) => x + sum(xs)
  }

  def product(ds: MyList[Double]): Double = ds match {
    case Nil => 1.0
    case Cons(0.0, _) => 0.0
    case Cons(x,xs) => x * product(xs)
  }
  def apply[A](as: A*): MyList[A] =
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))

  @annotation.tailrec
  def foldLeft[A,B](l: MyList[A], z: B)(f: (B, A) => B): B = l match {
    case Nil => z
    case Cons(h,t) => foldLeft(t, f(z,h))(f)
  }

  def foldRight[A,B](l: MyList[A], z: B)(f: (A,B) => B): B = l match {
    case Nil => z
    case Cons(h,t) => f(h, foldRight(t,z)(f))
  }
  def reverse[A](l:MyList[A]):MyList[A] = {
    foldLeft(l, Nil:MyList[A])((x, y)=> Cons(y,x))
  }
  def foldRightByLeft[A,B](l:MyList[A], z:B)(f: (A,B)=>B):B = {
    foldLeft(reverse(l),z)((x,y)=>f(y,x))
  }
  def appendRight[A](l:MyList[A], el:A):MyList[A] = {
    foldRight(l, Cons(el,Nil))((el,acc)=>Cons(el,acc))
  }
  def appendLeft[A](l:MyList[A], el:A):MyList[A] ={
    foldLeft(foldLeft(l,Nil:MyList[A])((x, y)=>Cons(y,x)),Cons(el,Nil))((x, y)=> Cons(y,x))
  }
  def map[A,B](l:MyList[A])(f:A=>B):MyList[B] = {
    reverse(foldLeft(l, Nil:MyList[B])((acc, el)=>Cons(f(el),acc)))
  }
  def flatMap[A,B](l:MyList[A])(f:A=>MyList[B]):MyList[B] = {
    concat(map(l)(f))
  }
  def filter[A](as: MyList[A])(f: A => Boolean): MyList[A]={
    reverse(foldLeft(as, Nil:MyList[A])((acc, el)=>if (f(el)) Cons(el, acc) else acc))
  }
  def concat[A](l:MyList[MyList[A]]):MyList[A]={
    reverse(foldLeft(l, Nil:MyList[A])((acc, el)=>foldLeft(el, acc)((acc1, el1)=>Cons(el1,acc1))))
  }
  def startsWith[A](l:MyList[A], start_l:MyList[A]):Boolean={
    (l, start_l) match{
      case (_, Nil) => true
      case (Nil,Cons(h,t))=> false
      case (Cons(h1,t1), Cons(h2,t2))=>if (h1!=h2) false else startsWith(t1,t2)
    }
  }
  def hasSubSeq[A](l:MyList[A], sub_l:MyList[A]):Boolean = {
    (l, startsWith(l, sub_l)) match {
      case (_,true) => true
      case (Nil,_) => false
      case (Cons(h,t),_)=> hasSubSeq(t,sub_l)
    }
  }

}