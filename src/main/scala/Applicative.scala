
object Applicative {
    trait Applicative[F[_]] extends Functor[F] {
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



}
