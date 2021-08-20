package Jane

import scala.io.Source

object StringParser {
    def str2ListInt(input:String, sep: String = " "):List[Int] = {
        input match {
            case ""=> List.empty[Int]
            case _ => input.split(sep).filterNot(_ == "").map(_.toInt).toList
        }

    }
    def parseFile[A](fileName:String)(parser:String=>A):List[A] = {
        for {
            line <- Source.fromFile(fileName).getLines
        } yield parser(line)
    }.toList
}
