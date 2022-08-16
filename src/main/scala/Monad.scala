import Applicative.Applicative
import Par.Par
import testing._

trait Functor[F[_]] {
    def map[A,B](fa: F[A])(f: A => B): F[B]
    def distribute[A,B](fab: F[(A, B)]): (F[A], F[B]) =
        (map(fab)(_._1), map(fab)(_._2))

}


trait Monad[F[_]] extends Functor[F] with Applicative[F]{
    def unit[A](a: => A): F[A]
    def flatMap[A,B](ma: F[A])(f: A => F[B]): F[B]
    override def map[A,B](ma:F[A])(f:A=>B):F[B] = flatMap(ma)(x=>unit(f(x)))
    def map2[A,B,C](ma: F[A], mb: F[B])(f: (A, B) => C): F[C] =
        flatMap(ma)(a => map(mb)(b => f(a, b)))

    def filterM[A](ms: List[A])(f: A => F[Boolean]): F[List[A]] = {
        val r1 = ms.map(x => map2(unit(x), f(x))((_,_)))
        map(sequence(r1))(_.filter(_._2).map(_._1))
    }
    def compose[A,B,C](f: A => F[B], g: B => F[C]): A => F[C] = {
        x:A => flatMap(f(x))(g)
    }
    def flatMap2[A,B](ma: F[A])(f: A => F[B]): F[B] = {
        compose((_:Unit) => ma, f)(())
    }
    def join[A](mma: F[F[A]]): F[A] = {
        flatMap(mma)(x=>x)
    }
    def compose2[A,B,C](f: A => F[B], g: B => F[C]): A => F[C] = {
        a:A => {
            val r1 = map(join(map(unit(a))(f)))(g)
            join(r1)
        }

    }



}

object Monad {
    val genMonad = new Monad[Gen] {
        def unit[A](a: => A): Gen[A] = Gen.unit(a)
        def flatMap[A,B](ma: Gen[A])(f: A => Gen[B]): Gen[B] =
            ma flatMap f
    }
    val parMonad = new Monad[Par] {
        override def unit[A](a: => A): Par[A] = Par.unit(a)

        override def flatMap[A, B](ma: Par[A])(f: A => Par[B]): Par[B] = Par.flatMap(ma)(f)
    }
    val optionMonad = new Monad[Option] {
        override def unit[A](a: => A): Option[A] = Option(a)

        override def flatMap[A, B](ma: Option[A])(f: A => Option[B]): Option[B] = ma flatMap f
    }

    val streamMonad = new Monad[Stream] {
        override def unit[A](a: => A): Stream[A] = Stream(a)

        override def flatMap[A, B](ma: Stream[A])(f: A => Stream[B]): Stream[B] = ma flatMap f
    }

    val listMonad = new Monad[List] {
        override def unit[A](a: => A): List[A] = List(a)
        override def flatMap[A, B](ma: List[A])(f: A => List[B]): List[B] = ma flatMap f
    }
    def stateMonad[S]= new Monad[({type f[x] = State[S,x]})#f] {
        def unit[A](a: => A): State[S,A] = State.unit(a)
        def flatMap[A,B](st: State[S,A])(f: A => State[S,B]): State[S,B] = st.flatMap(f)
    }


}

