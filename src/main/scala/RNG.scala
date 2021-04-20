
trait RNG {
  def nextInt: (Int, RNG)
}




object State {
  type State[S,+A] = S => (A,S)
//  unit, map, map2, flatMap, and sequence
  def unit[S,A](a:A):State[S,A]=
    s=>(a,s)
  def flatMap[S,A,B](st:State[S,A])(f:A=>State[S,B]):State[S,B]=
   s=>{
     val (a,s1) = st(s)
     f(a)(s1)
   }

  def map[S,A,B](st:State[S,A])(f:A => B):State[S,B]=
    flatMap(st)(x=>unit(f(x)))

  def map2[S,A,B,C](st1:State[S,A], st2:State[S,B])(f:(A,B)=>C):State[S,C]={
    flatMap(st1)(a=> map(st2)(b=>f(a,b)))
  }
  def sequence[S,A](fs: List[State[S,A]]): State[S,List[A]]={
    val r1:State[S,List[A]] = State.unit(List[A]())
    List.foldLeft(fs, r1)(map2(_,_)((x,y)=>Cons(y,x)))
  }
  def get[S]: State[S, S] = s => (s, s)

}

case class SimpleRNG(seed: Long) extends   RNG
{
  def nextInt: (Int, RNG) = {
    val newSeed = (seed * 0x5DEECE66DL + 0xBL) & 0xFFFFFFFFFFFFL
    val nextRNG = SimpleRNG(newSeed)
    val n = (newSeed >>> 16).toInt
    (n, nextRNG)
  }


}
object Rand {
  type Rand[+A] = RNG => (A, RNG)

  def randomPair(rng: RNG): (Int,Int) = {
    val (i1,_) = rng.nextInt
    val (i2,_) = rng.nextInt
    (i1,i2)
  }
  def nonNegativeInt(rng: RNG): (Int, RNG)={
    val (n, nextRNG) = rng.nextInt
    val nonNegativeN = n match {
      case Int.MinValue => 0
      case _ => math.abs(n)
    }
    (nonNegativeN, nextRNG)
  }
  def double(rng: RNG): (Double, RNG) ={
    val (nextInt, nextRNG) = nonNegativeInt(rng)
    (nextInt.toDouble/Int.MaxValue, nextRNG)
  }
  def intDouble(rng: RNG): ((Int,Double), RNG)={
    val (i1, rng2) = rng.nextInt
    val (i2, rng3) = double(rng2)
    ((i1, i2), rng3)
  }
  def doubleInt(rng: RNG): ((Double,Int), RNG)={
    val res = intDouble(rng)
    ((res._1._2, res._1._1), res._2)
  }
  def double3(rng: RNG): ((Double,Double,Double), RNG)={
    val (i1, rng2) = double(rng)
    val (i2, rng3) = double(rng2)
    val (i3, rng4) = double(rng3)
    ((i1,i2,i3), rng4)
  }
  def ints(count: Int)(rng: RNG): (List[Int], RNG)={
    def go(count:Int, acc:(List[Int], RNG)):(List[Int], RNG) = {
      count match {
        case 0 => acc
        case n => {
          val nextRand=  nonNegativeInt(acc._2)
          go(n-1, (Cons(nextRand._1, acc._1), nextRand._2))
        }
      }
    }
    go(count, (Nil, rng))
  }
  val int: Rand[Int] = _.nextInt
  def unit[A](a: A): Rand[A] =
    rng => (a, rng)

  def map[A,B](s: Rand[A])(f: A => B): Rand[B] =
    rng => {
      val (a, rng2) = s(rng)
      (f(a), rng2)
    }
  def nonNegativeEven: Rand[Int] =
    map(nonNegativeInt)(i => i - i % 2)

  def double2:Rand[Double]=
    map(nonNegativeInt)(_ / Int.MaxValue.toDouble)

  def map2[A,B,C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C]={
    rng=>{
      val (a1, rng2) = ra(rng)
      val (a2, rng3) = rb(rng2)
      (f(a1,a2), rng3)
    }
  }
  def both[A,B](ra: Rand[A], rb: Rand[B]): Rand[(A,B)] =
    map2(ra, rb)((_, _))

  def sequence[A](fs: List[Rand[A]]): Rand[List[A]]={
    List.foldLeft(fs, unit(List[A]()))(map2(_,_)((x,y)=> Cons(y,x)))
  }
  def flatMap[A,B](f: Rand[A])(g: A => Rand[B]): Rand[B]={
    rng=>{
      val (a, rng2)= f(rng)
      g(a)(rng2)
    }
  }
  def mapByF[A,B](s: Rand[A])(f: A => B): Rand[B] ={
    flatMap(s)(a=>unit(f(a)))
  }
  def map2ByF[A,B,C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C]={
    flatMap(ra)(a => map(rb)(b => f(a, b)))
  }



}
