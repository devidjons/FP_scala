package Jane
import WinningCombinations._
import MultipleSolutions._



object tests extends App{

    //solution
    val solution1 = Solution(List(10,10,10,20,20,20,10,0,0,0))
    val solution2 = Solution(List(10,10,10,20,20,19,9,0,0,0))
    val solution3 = Solution(List(10,10,10,20,20,21,9,0,0,0))
    println(solution1 <= solution2)
    println(solution2 <= solution1)
    println(solution1.compare(solution2))
    println(solution1.compare(solution1))
    println(solution1 & solution2)
    println(solution1 & solution3)
}