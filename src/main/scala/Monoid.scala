import Par.{map2, unit}
import testing.{Gen, Prop}

object Monoid {
    trait Monoid[A] {
        def op(a1: A, a2: A): A
        def zero: A
    }
    val stringMonoid = new Monoid[String] {
        def op(a1: String, a2: String) = a1 + a2
        val zero = ""
    }

    def listMonoid[A] = new Monoid[List[A]] {
        def op(a1: List[A], a2: List[A]) = a1 ++ a2
        val zero = List.empty[A]
    }

    val intAddition: Monoid[Int] = new Monoid[Int] {
        def op(a1: Int, a2: Int):Int = a1 + a2
        val zero = 0
    }
    val intMultiplication: Monoid[Int] =new Monoid[Int] {
        def op(a1: Int, a2: Int) = a1 * a2
        val zero = 1
    }

    val booleanOr: Monoid[Boolean] = new Monoid[Boolean] {
        override def op(a1: Boolean, a2: Boolean): Boolean = a1 || a2
        override def zero: Boolean = false
    }
    val booleanAnd: Monoid[Boolean] = new Monoid[Boolean] {
        override def op(a1: Boolean, a2: Boolean): Boolean = a1 && a2
        override def zero: Boolean = true
    }
    def optionMonoid[A]: Monoid[Option[A]] = new Monoid[Option[A]] {
        override def op(a1: Option[A], a2: Option[A]): Option[A] = a1.orElse(a2)
        override def zero: Option[A] = Option.empty[A]
    }


    def endoMonoid[A]: Monoid[A => A] = new Monoid[A => A] {
        override def op(a1: A => A, a2: A => A): A => A = a=> a2(a1(a))
        override def zero: A => A = x=>x
    }
    def monoidLaws[A](m: Monoid[A], gen: Gen[A]): Prop = {
        val law1 = (x:A)=> m.op(x, m.zero) == x && m.op(m.zero, x) == x
        val law2 = (x:A,y:A,z:A)=> m.op(m.op(x,y),z) == m.op(x, m.op(y,z))
        val prop1 = testing.Gen.forAll(gen)(law1)
        val gen3 = gen.flatMap(x=> gen.flatMap(y => gen.map(z=> (x,y,z))))
        val prop2 = testing.Gen.forAll(gen3)(law2.tupled)
        prop1 && prop2
    }
    def foldMap[A,B](as: List[A], m: Monoid[B])(f: A => B): B = as.map(f).foldLeft(m.zero)(m.op)
    def foldLeft[A,B](as:List[A], m: Monoid[B])(f:(B,A)=>B):B=foldMap(as, m)(x=>f(m.zero, x))
    def foldMapV[A,B](v: IndexedSeq[A], m: Monoid[B])(f: A => B): B = {
        v.size match {
            case 1 => f(v.head)
            case n=>{
                val (l,r) = v.splitAt(n/2)
                m.op(foldMapV(l,m)(f), foldMapV(r,m)(f))
            }
        }
    }
    import Par.Par

    def par[A](m: Monoid[A]): Monoid[Par[A]] = new Monoid[Par[A]] {
        override def op(a1: Par[A], a2: Par[A]): Par[A] = map2(a1,a2)(m.op)

        override def zero: Par[A] = unit(m.zero)
    }
    def parFoldMap[A,B](v: IndexedSeq[A], m: Monoid[B])(f: A => B): Par[B] = foldMapV(v, par(m))(x=>unit(f(x)))
    sealed trait seqLR {
       val isOrderred:Boolean


    }
    def mapMergeMonoid[K,V](V: Monoid[V]): Monoid[Map[K, V]] =
        new Monoid[Map[K, V]] {
            def zero = Map[K,V]()
            def op(a: Map[K, V], b: Map[K, V]) =
                (a.keySet ++ b.keySet).foldLeft(zero) { (acc,k) =>
                    acc.updated(k, V.op(a.getOrElse(k, V.zero),
                        b.getOrElse(k, V.zero)))
                }
        }


    def functionMonoid[A,B](mb: Monoid[B]): Monoid[A => B] = new Monoid[A=>B] {
        override def op(a1: A => B, a2: A => B): A => B = {
            a=> mb.op(a1(a), a2(a))
        }

        override def zero: A => B = _ => mb.zero
    }

    def bag[A](as: IndexedSeq[A]): Map[A, Int] = {
        foldMapV(as, mapMergeMonoid[A,Int](intAddition))(x=> Map(x->1))
    }



}



trait Foldable[F[_]] {
    import Monoid._
    def foldRight[A,B](as: F[A])(z: B)(f: (A,B) => B): B
    def foldLeft[A,B](as: F[A])(z: B)(f: (B,A) => B): B
    def foldMap[A,B](as: F[A])(f: A => B)(mb: Monoid[B]): B
    def concatenate[A](as: F[A])(m: Monoid[A]): A =
        foldLeft(as)(m.zero)(m.op)
    def toList[A](fa: F[A]): List[A] = {
        foldLeft(fa)(List.empty[A])((x,y)=>y::x).reverse
    }

}

object ListFoldable extends Foldable[List] {
    override def foldRight[A, B](as: List[A])(z: B)(f: (A, B) => B): B = as.foldRight(z)(f)

    override def foldLeft[A, B](as: List[A])(z: B)(f: (B, A) => B): B = as.foldLeft(z)(f)

    override def foldMap[A, B](as: List[A])(f: A => B)(mb: Monoid.Monoid[B]): B = Monoid.foldMap(as, mb)(f)
}

object IndexedSeqFoldable extends Foldable[IndexedSeq] {
    override def foldRight[A, B](as: IndexedSeq[A])(z: B)(f: (A, B) => B): B = as.foldRight(z)(f)

    override def foldLeft[A, B](as: IndexedSeq[A])(z: B)(f: (B, A) => B): B = as.foldLeft(z)(f)

    override def foldMap[A, B](as: IndexedSeq[A])(f: A => B)(mb: Monoid.Monoid[B]): B = Monoid.foldMapV(as, mb)(f)
}

object StreamFoldable extends Foldable[Stream]{
    override def foldRight[A, B](as: Stream[A])(z: B)(f: (A, B) => B): B = as.foldRight(z)(f)

    override def foldLeft[A, B](as: Stream[A])(z: B)(f: (B, A) => B): B = as.foldLeft(z)(f)

    override def foldMap[A, B](as: Stream[A])(f: A => B)(mb: Monoid.Monoid[B]): B = as.map(f).foldLeft(mb.zero)(mb.op)
}

object TreeFoldable extends Foldable[Tree]{
    import Tree._
    override def foldRight[A, B](as: Tree[A])(z: B)(f: (A, B) => B): B =
        as match {
            case Leaf(value)=> f(value, z)
            case Branch(l,r) => foldRight(l)(foldRight(r)(z)(f))(f)
        }

    override def foldLeft[A, B](as: Tree[A])(z: B)(f: (B, A) => B): B =
        as match {
            case Leaf(value)=> f(z,value)
            case Branch(l,r) => foldLeft(r)(foldLeft(l)(z)(f))(f)
        }
    override def foldMap[A, B](as: Tree[A])(f: A => B)(mb: Monoid.Monoid[B]): B =
        fold[B,B](map(as)(f),x=>x)(mb.op)
}

object OptionFoldable extends Foldable[Option] {
    override def foldRight[A, B](as: Option[A])(z: B)(f: (A, B) => B): B = as.map(f(_,z)).getOrElse(z)

    override def foldLeft[A, B](as: Option[A])(z: B)(f: (B, A) => B): B = as.map(f(z,_)).getOrElse(z)

    override def foldMap[A, B](as: Option[A])(f: A => B)(mb: Monoid.Monoid[B]): B = as.map(f).getOrElse(mb.zero)
}

object testBag extends App {
    import Monoid._
    println(bag(IndexedSeq(1,2,3,4,53,2,1)))
}
//Implement Foldable[List], Foldable[IndexedSeq], and Foldable[Stream] .
//Remember that foldRight, foldLeft, and foldMap can all be implemented in terms
//    of each other, but that might not be the most efficient implementation.

