package Jane

import WinningCombinations._
import MultipleSolutions._


object tests extends App {

    //solution
    val solution1 = Solution(List(10, 10, 10, 20, 20, 20, 10, 0, 0, 0))
    val solution2 = Solution(List(10, 10, 10, 20, 20, 19, 9, 0, 0, 0))
    val solution3 = Solution(List(10, 10, 10, 20, 20, 21, 9, 0, 0, 0))
    println(solution1 <= solution2)
    println(solution2 <= solution1)
    println(solution1.compare(solution2))
    println(solution1.compare(solution1))
    println(solution1 & solution2)
    println(solution1 & solution3)
    val points = List(
        List(33, 33, 34, 0, 0, 0, 0, 0, 0, 0),
        List(0, 0, 33, 33, 34, 0, 0, 0, 0, 0),
        List(0, 20, 50, 30, 0, 0, 0, 0, 0, 0),
        List(98, 1, 1, 0, 0, 0, 0, 0, 0, 0),
        List(0, 98, 1, 1, 0, 0, 0, 0, 0, 0),
        List(0, 0, 98, 1, 1, 0, 0, 0, 0, 0),
        List(0, 0, 0, 98, 1, 1, 0, 0, 0, 0),
        List(0, 0, 0, 0, 98, 1, 1, 0, 0, 0),
    )
    val r1 = points.map(x => generateSolutionsForStrategy(Strategy(x)).takeBest(3))
    //    println(r1)
//    println(r1.reduce(_ & _))
    println(generateBestStrategy(points.map(Strategy)).takeBest(3))


}