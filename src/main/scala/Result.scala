import Applicative.Traverse.listTraverse

import scala.collection.immutable._



object Result  extends App{

    /*
     * Complete the 'box' function below.
     *
     * The function is expected to return a LONG_INTEGER.
     * The function accepts following parameters:
     *  1. INTEGER n
     *  2. INTEGER m
     *  3. INTEGER_ARRAY h
     *  4. INTEGER_ARRAY v
     */
//
//    def box(n: Int, m: Int, h: Array[Int], v: Array[Int]): Long = {
//        val hMax= longestGap(h.sorted)
//        val vMax = longestGap(v.sorted)
//        (hMax+1)*(vMax+1)
//        // Write your code here
//
//    }
//    def longestGap(inp:Array[Int]):Int={
//
//        var result = 0
//        var prev = -2
//        var currResult = 0
//        for (el <- inp){
//            if (el-prev==1){
//                currResult+=1
//                prev = el
//            } else {
//                if (currResult>=result) result = currResult
//                prev = el
//                currResult = 1
//            }
//        }
//        if (currResult>result) result = currResult
//        result
//    }
//    case class currState(bestResult:Int=0, currResult:Int=0, prevEl:Int= -2){
//        def updated(el:Int):currState = {
//            if (el - prevEl== -1){
//                currState(max(bestResult, currResult+1), currResult+1, el)
//            }
//            else {
//                currState(max(bestResult, currResult),1, el)
//            }
//        }
//    }
//    def longGap2(inp:Array[Int]):Int={
//        inp.foldLeft(currState())((state, el)=>{
//             state.updated(el)
//        }).bestResult
//
//    }
//
//    println(longestGap(Array(1,2,3)))
    val r1 = List(1,2,3,4)
    print(listTraverse.zipWithIndex(r1))

}
