
import testing.Gen.forAll
import testing._

import scala.{::, math}
import scala.collection.immutable.NumericRange.Exclusive

object testing {

    case class Gen[+A](sample: State[RNG,A]){
        def flatMap[B](f: A => Gen[B]): Gen[B]={
            val r1 = State((rng:RNG) => f(sample.run(rng)._1).sample.run(rng))
            Gen(r1)
        }
        def map[B](f:A=>B):Gen[B] = this.flatMap(x=> Gen.unit(f(x)))
        def listOfNbyF(size: Gen[Int]): Gen[List[A]] = size.flatMap(n=> Gen.listOfN(n,this))

        def unsized: SGen[A] = SGen(_ => this)


    }
    object Gen{
        def choose(start:Int, endExclusive:Int):Gen[Int]={
            Gen(State(Rand.nonNegativeInt).map(x=>x % (endExclusive-start) + start))
        }
        def unit[A](a: => A): Gen[A] = {
            Gen(State.unit(a))
        }
        def boolean:Gen[Boolean]={
            val st1:State[RNG, Int] = State((rng:RNG)=> rng.nextInt)
            Gen(State.map(st1)(_%2 == 0))
        }
        def listOfN[A](n: Int, g: Gen[A]): Gen[List[A]]={
            val r1:List[State[RNG,A]] = List.fill(n)(g.sample)
            Gen(State.sequence(r1))
        }

        def union[A](g1: Gen[A], g2: Gen[A]): Gen[A]= {
            choose(1,3).flatMap({
                case 1 => g1
                case _ => g2
            })
        }
        def weighted[A](g1: (Gen[A],Double), g2: (Gen[A],Double)): Gen[A] = {
            choose(0,Int.MaxValue).flatMap(x=>{
                val p = x.toDouble/Int.MaxValue
                val p1 = g1._2 / (g1._2 + g2._2)
                if (p<p1) g1._1 else g2._1
            } )
        }

        def forAll[A](as: Gen[A])(f: A => Boolean): Prop = Prop {
            (_,n,rng) => randomStream(as)(rng).zipWith(MyStream.from(0))((_,_)).take(n).map {
                case (a, i) => try {
                    if (f(a)) Passed else Falsified(a.toString, i)
                } catch { case e: Exception => Falsified(buildMsg(a, e), i) }
            }.find(_.isFalsified).getOrElse(Passed)
        }

        def forAll[A](g: SGen[A])(f: A => Boolean): Prop =
            forAll(g(_))(f)
        def forAll[A](g: Int => Gen[A])(f: A => Boolean): Prop = Prop {
            (max,n,rng) =>
                val casesPerSize = (n + (max - 1)) / max
                val props: Stream[Prop] =
                    Stream.from(0).take((math.min(n,max)) + 1).map(i => forAll(g(i))(f))
                val prop: Prop =
                    props.map(p => Prop { (max, _, rng) =>
                        p.run(max, casesPerSize, rng)}).toList.reduce(_ && _)
            prop.run(max,n,rng)
        }


        def randomStream[A](g: Gen[A])(rng: RNG): MyStream[A] =
        MyStream.unfold(rng)(rng => Some(g.sample(rng)))
        def buildMsg[A](s: A, e: Exception): String =
            s"test case: $s\n" +
                s"generated an exception: ${e.getMessage}\n" +
            s"stack trace:\n ${e.getStackTrace.mkString("\n")}"

    }

    type TestCases = Int
    type MaxSize = Int

    type SuccessCount = Int
    type FailedCase = String

    case class Prop(run: (MaxSize, TestCases,RNG) => Result){
        def &&(p: Prop): Prop = Prop {
            (MaxSize, testCases, rng) => this.run(MaxSize, testCases, rng) match {
                case Falsified(f,s)=> Falsified(f,s)
                case Passed => p.run(MaxSize, testCases, rng)
            }
        }
//        def ||(p: Prop): Prop = Prop {
//            (testCases, rng) => this.run(testCases, rng) match {
//                case Falsified(f,s)=> p.run(testCases, rng)
//                case Passed => Passed
//            }
//        }
    }

    def run(p: Prop,
        maxSize: Int = 100,
        testCases: Int = 100,
        rng: RNG = SimpleRNG(System.currentTimeMillis)): Unit =
        p.run(maxSize, testCases, rng) match {
            case Falsified(msg, n) =>
                println(s"! Falsified after $n passed tests:\n $msg")
            case Passed =>
                println(s"+ OK, passed $testCases tests.")
        }




    sealed trait Result {
        def isFalsified: Boolean

    }
    case object Passed extends Result {
        def isFalsified = false
    }
    case class Falsified(failure: FailedCase,
        successes: SuccessCount) extends Result {
        def isFalsified = true
    }

    case class SGen[+A](forSize: Int => Gen[A]){
        def apply(n: Int): Gen[A] = this.forSize(n)

        def flatMap[B](f: A => SGen[B]): SGen[B]={
            SGen(n=>this(n).flatMap(a=>f(a)(n)))
        }

        def map[B](f:A => B):SGen[B] = SGen {
            n=> this(n).map(f)
        }
    }

    object SGen {
        def listOf[A](g: Gen[A]): SGen[List[A]]=
            SGen(n=>Gen.listOfN(n,g))

        def listOf1[A](g: Gen[A]): SGen[List[A]]=
            SGen(n=>Gen.listOfN(scala.math.max(n,1),g))
    }






}

object testMonoid extends App {
    val smallInt = Gen.choose(-10,10)
    val maxProp = forAll(SGen.listOf1(smallInt)) { ns =>
        val max = ns.max
        !ns.exists(_ > max)
    }
    run(maxProp)
}
