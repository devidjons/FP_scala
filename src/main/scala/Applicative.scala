import scala.Option
import scala.Some
object Applicative {
    trait Applicative[F[_]] extends Functor[F] { self =>
        // primitive combinators
        def map2[A,B,C](fa: F[A], fb: F[B])(f: (A, B) => C): F[C]
        def unit[A](a: => A): F[A]
        def map[A,B](fa: F[A])(f: A => B): F[B] = map2(fa, unit(()))((a, _) => f(a))
        def apply[A,B](fab: F[A => B])(fa: F[A]): F[B]= map2(fab, fa)((a,b)=> a(b))
        def traverse[A,B](as: List[A])(f: A => F[B]): F[List[B]] =
            as.foldRight(unit(List[B]()))((a, fbs) => map2(f(a), fbs)(_ :: _))
        def sequence[A](fas: List[F[A]]): F[List[A]] = traverse(fas)(x=>x)
        def replicateM[A](n: Int, fa: F[A]): F[List[A]] = sequence(List.fill(n)(fa))
        def product[A,B](fa: F[A], fb: F[B]): F[(A,B)] = map2(fa,fb)((_,_))
        def product[G[_]](G: Applicative[G]): Applicative[({type f[x] = (F[x], G[x])})#f]={
            new Applicative[({type f[x] = (F[x], G[x])})#f] {
                override def map2[A, B, C](fa: (F[A], G[A]), fb: (F[B], G[B]))(f: (A, B) => C): (F[C], G[C]) =
                    (self.map2(fa._1, fb._1)(f), G.map2(fa._2, fb._2)(f))

                override def unit[A](a: => A): (F[A], G[A]) = (self.unit(a), G.unit(a))
            }
        }
        def compose[G[_]](G: Applicative[G]): Applicative[({type f[x] = F[G[x]]})#f]={
            new Applicative[({type f[x] = F[G[x]]})#f] {
                override def map2[A, B, C](fa: F[G[A]], fb: F[G[B]])(f: (A, B) => C): F[G[C]] =
                    self.map2(fa,fb)((ga,gb)=> G.map2(ga,gb)(f))

                override def unit[A](a: => A): F[G[A]] = self.unit(G.unit(a))
            }
        }
        def sequenceMap[K,V](ofa: Map[K,F[V]]): F[Map[K,V]]= {
            ofa.foldLeft(unit(Map[K,V]()))((a,b)=>{
                map2(a,b._2)((x,y)=>x.updated(b._1, y))
            })
        }





    }
    trait Applicative2[F[_]] extends Functor[F] {
        def apply[A,B](fab: F[A => B])(fa: F[A]): F[B]
        def unit[A](a: => A): F[A]
        def map[A,B](fa: F[A])(f: A => B): F[B] = apply(unit(f))(fa)
        def map2[A,B,C](fa: F[A], fb: F[B])(f: (A,B) => C): F[C]={
            val g = unit(f.curried)
            apply(apply(g)(fa))(fb)
        }
        def map3[A,B,C,D](fa: F[A],
                          fb: F[B],
                          fc: F[C])(f: (A, B, C) => D): F[D] = {
            val g = unit(f.curried)
            apply(apply(apply(g)(fa))(fb))(fc)
        }
        def map4[A,B,C,D,E](fa:F[A],
                            fb:F[B],
                            fc:F[C],
                            fd:F[D])(f: (A, B, C, D) => E): F[E] = {
            val g: ((A,B,C)=>D=>E) = (a,b,c)=>(d)=> f(a,b,c,d)
            apply(map3(fa,fb,fc)(g))(fd)
        }

    }

    def eitherMonad[E]: Monad[({type f[x] = Either[E, x]})#f] = {
        new Monad[({type f[x] = Either[E, x]})#f] {
            override def unit[A](a: => A): Either[E, A] = Right(a)
            override def flatMap[A, B](ma: Either[E, A])(f: A => Either[E, B]): Either[E, B] = ma.flatMap(f)
        }
    }
    val applicativeOption = new Applicative[MyOption] {
        override def map2[A, B, C](fa: MyOption[A], fb: MyOption[B])(f: (A, B) => C): MyOption[C] = fa.flatMap(a=>fb.map(f(a,_)))

        override def unit[A](a: => A): MyOption[A] = Some(a)
    }
    trait Traverse[F[_]] extends Functor[F] {
        def traverse[G[_],A,B](fa: F[A])(f: A => G[B])(
            implicit G: Applicative[G]): G[F[B]] =
            sequence(map(fa)(f))
        def sequence[G[_],A](fga: F[G[A]])(
            implicit G: Applicative[G]): G[F[A]] =
            traverse(fga)(ga => ga)
        def map[A,B](fa: F[A])(f: A => B): F[B] = {
            case class idd[A](x:A)
            val app = new Applicative[idd] {
                override def map2[A, B, C](fa: idd[A], fb: idd[B])(f: (A, B) => C): idd[C] = idd(f(fa.x, fb.x))

                override def unit[A](a: => A): idd[A] = idd(a)
            }
            val G_f = (x:A) => app.unit(f(x))
            traverse(fa)(G_f)(app).x
        }
        def traverseS[S,A,B](fa: F[A])(f: A => State[S, B]): State[S, F[B]] =
            traverse[({type f[x] = State[S,x]})#f,A,B](fa)(f)(Monad.stateMonad)

        def zipWithIndex[A](ta: F[A]): F[(A,Int)] =
            traverseS(ta)((a: A) => (for {
                i <- State.get[Int]
                _ <- State.set(i + 1)
            } yield (a, i))).run(0)._1

    }



    object Traverse {
        val listTraverse = new Traverse[List] {
            override def traverse[G[_]:Applicative, A, B](fa: List[A])(f: A => G[B]): G[List[B]] = {
                val G = implicitly[Applicative[G]]
                G.sequence(fa.map(f))
            }
        }
        val optionTraverse = new Traverse[MyOption] {
            override def traverse[G[_] : Applicative, A, B](fa: MyOption[A])(f: A => G[B]): G[MyOption[B]] = {
                val G = implicitly[Applicative[G]]
                fa map f match {
                    case Some(value)=> G.map(value)(x=>Some(x))
                    case None => G.unit(None)
                }
            }
        }
        val TreeTraverse = new Traverse[Tree] {
            override def traverse[G[_] : Applicative, A, B](fa: Tree[A])(f: A => G[B]): G[Tree[B]] = {
                val G = implicitly[Applicative[G]]
                val f1:(A=>G[Tree[B]]) = (a:A)=>G.map(f(a))(Leaf(_))
                Tree.fold(fa,f1)((x,y)=>G.map2(x,y)(Branch(_,_)))

            }
        }

    }








}