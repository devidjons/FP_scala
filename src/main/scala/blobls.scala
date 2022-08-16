import scala.math.abs

object blobls extends App{
    def binom(n:Int, k:Int):Long = {
        k match {
            case 0=> 1
            case x if (x==n) =>1
            case _ =>binom(n-1,k) + binom(n-1,k-1)
        }
    }

    case class decisionInfo(decision: String, cost:Double)
    def initialStart():Map[(Int, Int), decisionInfo] = {
        val n = 30
        val r1 = for {
            k <- 0 to 30
        } yield {
            val a = 1.0
            val b = math.pow(3,k)/math.pow(2,n)
            val p_f = a/(a+b)
            val p_c = b/(a+b)
            if (k==2){
                println(s"$a, $b, pf = $p_f, pc = $p_c, ${p_f>p_c}")
            }
            val (decision, p) = if (p_f>p_c) ("fair", p_f) else ("cheater", p_c)
            val E_cost = n + p*(-15) + (1-p)*30
            (n,k)->(decisionInfo(decision, E_cost))
        }
        r1.toMap
    }
    def backwardStep(n:Int, k:Int, curr_results:Map[(Int,Int), decisionInfo]):decisionInfo = {
        val a = 1.0
        val b = math.pow(3,k)/math.pow(2,n)
        val p_f = a/(a+b)
        val p_c = b/(a+b)
        val (decision, p) = if (p_f>p_c) ("fair", p_f) else ("cheater", p_c)
        val E_cost = n + p*(-15) + (1-p)*30
        val continue_cost = (p_c*0.75+p_f*0.5) * curr_results((n+1,k+1)).cost + (0.25*p_c + 0.5*p_f)*curr_results((n+1,k)).cost
        if (continue_cost<E_cost) {
            decisionInfo("throw", continue_cost)
        } else {
            decisionInfo(decision, E_cost)
        }
    }
    def fullFillTable(n:Int, currSolutions:Map[(Int, Int), decisionInfo]):Map[(Int, Int), decisionInfo] = {
        if (n==0){
            currSolutions
        } else {
            val r1 = for {
                k <- 0 to n
            } yield {
                (n,k)->backwardStep(n,k, currSolutions)
            }
            fullFillTable(n-1, r1.toMap ++ currSolutions)
        }
    }
    val filledResults = fullFillTable(29, initialStart())
    for {
        n <- 1 to 30
        k <- 0 to n
    } yield {
        println(s"for $n throw and $k head desicions is ${filledResults(n,k)}")
    }

}
object Return3d extends App {
    def f(x:Int,y:Int,z:Int,m:Int): Double = {
        val dist =(abs(x)+abs(y)+abs(z))
        dist match {
            case d if d<0.001 => 1.0
            case d if (d>m) => 0.0
            case _ => {
                val step = if (m%2==0) 1 else -1
                (f(x+ step,y,z,m-1) + f(x,y+step,z,m-1)+f(x,y,z+step,m-1))/3.0
            }
        }
    }
    println(f(1,0,0,19))
    println(f(0,0,0,3))
}