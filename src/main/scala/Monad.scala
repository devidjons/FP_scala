import Par.Par
import State.State
import testing._

trait Functor[F[_]] {
    def map[A,B](fa: F[A])(f: A => B): F[B]
    def distribute[A,B](fab: F[(A, B)]): (F[A], F[B]) =
        (map(fab)(_._1), map(fab)(_._2))

}


trait Monad[F[_]] extends Functor[F]{
    def unit[A](a: => A): F[A]
    def flatMap[A,B](ma: F[A])(f: A => F[B]): F[B]
    def map[A,B](ma: F[A])(f: A => B): F[B] =
            flatMap(ma)(a => unit(f(a)))
    def map2[A,B,C](ma: F[A], mb: F[B])(f: (A, B) => C): F[C] =
        flatMap(ma)(a => map(mb)(b => f(a, b)))

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
//    class monad[T][Q] extends Monad[State[T,Q]] {
//        override def unit[A](a: => A): State[T,A] = State.unit[T,A](a)
//
//        override def flatMap[A, B](ma: State[T,A])(f: A => State[T,B]): State[T,B] = ???
//    }
}

