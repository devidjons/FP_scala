sealed trait List[+A]
case object Nil extends List[Nothing]
case class Cons[+A](head: A, tail: List[A]) extends List[A]
object List {
  def sum(ints: List[Int]): Int = ints match {
    case Nil => 0
    case Cons(x,xs) => x + sum(xs)
  }

  def product(ds: List[Double]): Double = ds match {
    case Nil => 1.0
    case Cons(0.0, _) => 0.0
    case Cons(x,xs) => x * product(xs)
  }
  def apply[A](as: A*): List[A] =
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))

  @annotation.tailrec
  def foldLeft[A,B](l: List[A], z: B)(f: (B, A) => B): B = l match {
    case Nil => z
    case Cons(h,t) => foldLeft(t, f(z,h))(f)
  }

  def foldRight[A,B](l: List[A], z: B)(f: (A,B) => B): B = l match {
    case Nil => z
    case Cons(h,t) => f(h, foldRight(t,z)(f))
  }
  def reverse[A](l:List[A]):List[A] = {
    foldLeft(l, Nil:List[A])((x,y)=> Cons(y,x))
  }
  def foldRightByLeft[A,B](l:List[A], z:B)(f: (A,B)=>B):B = {
    foldLeft(reverse(l),z)((x,y)=>f(y,x))
  }
  def appendRight[A](l:List[A], el:A):List[A] = {
    foldRight(l, Cons(el,Nil))((el,acc)=>Cons(el,acc))
  }
  def appendLeft[A](l:List[A],el:A):List[A] ={
    foldLeft(foldLeft(l,Nil:List[A])((x,y)=>Cons(y,x)),Cons(el,Nil))((x,y)=> Cons(y,x))
  }
  def map[A,B](l:List[A])(f:A=>B):List[B] = {
    reverse(foldLeft(l, Nil:List[B])((acc, el)=>Cons(f(el),acc)))
  }
  def filter[A](as: List[A])(f: A => Boolean): List[A]={
    reverse(foldLeft(as, Nil:List[A])((acc, el)=>if (f(el)) Cons(el, acc) else acc))
  }
  def concat[A](l:List[List[A]]):List[A]={
    reverse(foldLeft(l, Nil:List[A])((acc,el)=>foldLeft(el, acc)((acc1, el1)=>Cons(el1,acc1))))
  }

}