sealed trait Tree[+A] { self =>
  def map[B](f:A=>B):Tree[B] = {
    Tree.map(self)(f)
  }
}
case class Leaf[A](value: A) extends Tree[A]
case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]
object Tree {
  def size[A](t:Tree[A]):Int = {
    t match {
      case Leaf(_)=> 1
      case Branch(l,r)=> 1+size(l)+size(r)
    }
  }
  def max(t:Tree[Int]):Int = {
    t match {
      case Leaf(v)=>v
      case Branch(l,r)=> max(l) max max(r)
    }
  }
  def depth[A](t:Tree[A]):Int={
    t match {
      case Leaf(_)=>1
      case Branch(l,r)=> 1+ depth(l) max depth(r)
    }
  }
  def map[A,B](t:Tree[A])(f:A=>B):Tree[B]={
    t match {
      case Leaf(v)=> Leaf(f(v))
      case Branch(l,r)=> Branch(map(l)(f), map(r)(f))
    }
  }
  def fold[A,B](t:Tree[A],fL:A=>B)(fB:(B,B)=>B):B = {
    t match {
      case Leaf(v)=> fL(v)
      case Branch(l,r)=>fB(fold(l,fL)(fB),fold(r,fL)(fB))
    }
  }
}