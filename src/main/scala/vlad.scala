import Applicative.{Applicative}

object vlad extends App {
    val lo:List[MyOption[Int]] = List(Some(1),Some(2), Some(3))
    implicit val MyOptionApplicative = new Applicative[MyOption] {
        override def map2[A, B, C](fa: MyOption[A], fb: MyOption[B])(f: (A, B) => C): MyOption[C] ={
            fa.flatMap(a=>fb.map(b => f(a,b)))
        }

        override def unit[A](a: => A): MyOption[A] = Some(a)
    }
}

object quizgame extends App{
    val N = 40
    val points:List[Double] = List(0,1,2,5,10,20,30,40,50)
    val p = 0.8
    def nextStep(currPointIdx:Int, stepsLeft:Int, decisions:Map[(Int,Int),(Double,Boolean)]):(Double, Boolean) = {
        val state = decisions.mapValues(_._1)
        if (stepsLeft==0) (points(currPointIdx), true)
        else {
            if (currPointIdx==(points.length-1)) (points(currPointIdx)+p*state((1,stepsLeft-1)) + (1-p)*state((0,stepsLeft-1)),true)
            else {
                val trueReturn = (points(currPointIdx)+p*state((1,stepsLeft-1)) + (1-p)*state((0,stepsLeft-1)),true)
                val falseReturn = (p*state((currPointIdx+1,stepsLeft-1)) + (1-p)*state((0,stepsLeft-1)),false)
                if (trueReturn._1>falseReturn._1) trueReturn else falseReturn
            }
        }
    }
    val result = (0 to N).foldLeft(Map.empty[(Int, Int), (Double, Boolean)])((acc, steps)=>{
        val newDecisions = for {
            startPoint <- 0 until  points.length
        } yield {
            (startPoint, steps) -> nextStep(startPoint, steps, acc)
        }
        acc++newDecisions.toMap
    })
    result.map(x=>((points(x._1._1), x._1._2),x._2)).toList.sortBy(x=> -x._1._1*100+x._1._2).foreach(println)
}