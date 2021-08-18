package Jane

import scala.annotation.tailrec
import scala.math

object JaneStreet extends App {

}

final case class Solution(values: List[Int]) {
    def <=(other: Solution): Boolean = {
        values.zip(other.values).map({ case (x, y) => x <= y }).reduce(_ && _)
    }

    def &(other: Solution): Solution = Solution {
        values.zip(other.values).map({ case (a, b) => math.max(a, b) })
    }

    def compare(other: Solution): Int = {
        if (this <= other) 1 else if (other <= this) 2 else 0
    }

    @tailrec
    def insertIntoList(otherSolutions: List[Solution], acc: List[Solution] = Nil): List[Solution] =
        otherSolutions.headOption.map(compare) match {
            case None => this :: acc
            case Some(1) => this :: otherSolutions.tail ::: acc
            case Some(2) => otherSolutions ::: acc
            case Some(0) => insertIntoList(otherSolutions.tail, otherSolutions.head :: acc)
        }

    val minimalPoints = values.sum
}


case class MultipleSolutions(solutionsList: List[Solution]) {
    def &(other: MultipleSolutions): MultipleSolutions = MultipleSolutions {
        for {
            a1 <- solutionsList
            a2 <- other.solutionsList
        } yield a1 & a2
    }

    def dropWeak(): MultipleSolutions = MultipleSolutions {
        solutionsList.foldLeft(List.empty[Solution])((acc, el) => el.insertIntoList(acc))
    }

    def getMinSolution: Solution = solutionsList.minBy(_.minimalPoints)

    def isValid: Boolean = getMinSolution.minimalPoints <= 100

}


object MultipleSolutions {

    def generateSolutionsForStrategy(strategy: Strategy): MultipleSolutions = MultipleSolutions {
        import WinningCombinations.minimalWinningComparisons
        for {
            comparison <- minimalWinningComparisons
        } yield Solution {
            strategy.points.zip(comparison).map(x => pointsForSingleComparison(x._1, x._2))
        }
    }

    def pointsForSingleComparison(points: Int, comparison: Int): Int = {
        comparison match {
            case 1 => points + 1
            case 0 => points
            case -1 => 0
        }
    }


    def generateBestStrategy(strategiesList: List[Strategy]): MultipleSolutions = {
        strategiesList.map(generateSolutionsForStrategy).reduce((acc, el) => {
            val joined = (acc & el).dropWeak()
            if (joined.isValid) joined else acc
        })
    }
}

case class Strategy(points: List[Int]) {
    require(points.sum <= 100)
}


object WinningCombinations extends App {
    def isWinner(comparisons: List[Int]): Boolean = {
        require(comparisons.size == 10)
        if (winBy3(comparisons) != 0) winBy3(comparisons) == 1 else winByPoints(comparisons) == 1
    }

    @tailrec
    def winBy3(comparisons: List[Int]): Int = {
        comparisons.take(3) match {
            case Nil => 0
            case List(1, 1, 1) => 1
            case List(-1, -1, -1) => -1
            case _ => winBy3(comparisons.tail)
        }
    }

    def winByPoints(comparisons: List[Int]): Int = {
        val fieldPoints = 1.to(10).toList
        math.signum(comparisons.zip(fieldPoints).map(x => x._1 * x._2).sum)
    }

    val allPossibleComparisons = 1.to(10).foldLeft(List(List.empty[Int]))((acc, el) => {
        acc.flatMap(x => List(1 :: x, -1 :: x, 0 :: x))
    })
    val winningComparisons = allPossibleComparisons.filter(isWinner)
    val minimalWinningComparisons = MultipleSolutions(winningComparisons.map(Solution(_))).dropWeak().solutionsList.map(_.values)

}

