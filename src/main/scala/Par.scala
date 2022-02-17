
import java.util.concurrent._
import language.implicitConversions


object Par {
    type Par[A] = ExecutorService => Future[A]

    def run[A](s: ExecutorService)(a: Par[A]): Future[A] = a(s)

    def unit[A](a: A): Par[A] = (es: ExecutorService) => UnitFuture(a) // `unit` is represented as a function that returns a `UnitFuture`, which is a simple implementation of `Future` that just wraps a constant value. It doesn't use the `ExecutorService` at all. It's always done and can't be cancelled. Its `get` method simply returns the value that we gave it.

    private case class UnitFuture[A](get: A) extends Future[A] {
        def isDone = true

        def get(timeout: Long, units: TimeUnit) = get

        def isCancelled = false

        def cancel(evenIfRunning: Boolean): Boolean = false
    }

    def map2[A, B, C](a: Par[A], b: Par[B])(f: (A, B) => C): Par[C] = // `map2` doesn't evaluate the call to `f` in a separate logical thread, in accord with our design choice of having `fork` be the sole function in the API for controlling parallelism. We can always do `fork(map2(a,b)(f))` if we want the evaluation of `f` to occur in a separate thread.
        (es: ExecutorService) => {
            val af = fork(a)(es)
            val bf = fork(b)(es)
            UnitFuture(f(af.get, bf.get)) // This implementation of `map2` does _not_ respect timeouts, and eagerly waits for the returned futures. This means that even if you have passed in "forked" arguments, using this map2 on them will make them wait. It simply passes the `ExecutorService` on to both `Par` values, waits for the results of the Futures `af` and `bf`, applies `f` to them, and wraps them in a `UnitFuture`. In order to respect timeouts, we'd need a new `Future` implementation that records the amount of time spent evaluating `af`, then subtracts that time from the available time allocated for evaluating `bf`.
        }

    def map[A,B](pa: Par[A])(f: A => B): Par[B] =
        map2(pa, unit(()))((a,_) => f(a))

    def fork[A](a: => Par[A]): Par[A] = // This is the simplest and most natural implementation of `fork`, but there are some problems with it--for one, the outer `Callable` will block waiting for the "inner" task to complete. Since this blocking occupies a thread in our thread pool, or whatever resource backs the `ExecutorService`, this implies that we're losing out on some potential parallelism. Essentially, we're using two threads when one should suffice. This is a symptom of a more serious problem with the implementation, and we will discuss this later in the chapter.
        es => es.submit(new Callable[A] {
            def call = a(es).get
        })

    def asyncF[A,B](f: A => B): A => Par[B]={
        a=> unit(f(a))
    }
    def sequence[A](ps: List[Par[A]]): Par[List[A]]={
        ps.foldLeft(unit(List.empty[A]))((acc, el)=>map2(el, acc)(_::_))
    }

    def sequenceFilter[A](ps: List[Par[(A, Boolean)]]): Par[List[A]]={
        ps.foldLeft(unit(List.empty[A]))((acc, el)=>map2(el, acc)( (el, acc)=>if (el._2) el._1 :: acc else acc))

    }

    def parMap[A,B](ps: List[A])(f: A => B): Par[List[B]] = fork {
        val fbs: List[Par[B]] = ps.map(asyncF(f))
        sequence(fbs)
    }


    def parFilter[A](as: List[A])(f: A => Boolean): Par[List[A]]={
        fork{
            sequenceFilter(as.map(asyncF(x=>(x, f(x)))))
        }
    }
    def reduce[A](lst:IndexedSeq[A], default: A)(f:(A,A)=>A):A={
        if (lst.size <= 1) lst.headOption.getOrElse(default)
        else {
            val (l,r) = lst.splitAt(lst.length/2)
            f(reduce(l, default)(f), reduce(r, default)(f))
        }
    }
    def reduce2[A](ints: IndexedSeq[A], default:A)(f:(A,A)=>A): Par[A] = {
        if (ints.size <= 1)
            Par.unit(ints.headOption.getOrElse(default))
        else {
            val (l,r) = ints.splitAt(ints.length/2)
            val resuqltL: Par[A] = reduce2(l, default)(f)
            val resultR: Par[A] = reduce2(r, default)(f)
            map2(resuqltL, resultR)(f(_,_))
        }
    }
    def max(ints:IndexedSeq[Int]):Int = reduce(ints, ints.head)(math.max)

    def choice[A](cond: Par[Boolean])(t: Par[A], f: Par[A]): Par[A] =
        es =>
            if (run(es)(cond).get) t(es) // Notice we are blocking on the result of `cond`.
            else f(es)

    def choiceN[A](n: Par[Int])(choices: List[Par[A]]): Par[A] =
        es => {
            val ind = run(es)(n).get // Full source files
            run(es)(choices(ind))
        }


    def choiceMap[K,V](key: Par[K])(choices: Map[K,Par[V]]): Par[V] =
        es => {
            val k = run(es)(key).get
            run(es)(choices(k))
        }

    def chooser[A,B](p: Par[A])(choices: A => Par[B]): Par[B] =
        es => {
            val k = run(es)(p).get
            run(es)(choices(k))
        }

    /* `chooser` is usually called `flatMap` or `bind`. */
    def flatMap[A,B](p: Par[A])(choices: A => Par[B]): Par[B] =
        es => {
            val k = run(es)(p).get
            run(es)(choices(k))
        }

    def choiceViaFlatMap[A](p: Par[Boolean])(f: Par[A], t: Par[A]): Par[A] =
        flatMap(p)(b => if (b) t else f)

    def choiceNViaFlatMap[A](p: Par[Int])(choices: List[Par[A]]): Par[A] =
        flatMap(p)(i => choices(i))

    def join[A](a:Par[Par[A]]):Par[A] = {
        es =>a(es).get.apply(es)
    }
    def joinViaFlatMap[A](a:Par[Par[A]]):Par[A] = {
        flatMap(a)(x=>x)
    }
    def flatMapViaJoin[A,B](a:Par[A])(f:A=>Par[B]):Par[B] = {
        join(map(a)(f))
    }



}

object test123 extends App{
    val r1 = {
        a:Int => a+1
        2
    }
    println(r1(2))
}
