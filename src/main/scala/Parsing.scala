import Parsing.JSON.JNull
import Parsing._

import scala.util.matching.Regex

object Parsing {
    type Parser[+T] = Function1[String,(Option[(T, String)], ParseError)]
    case class Location(input: String, offset: Int = 0) {
        lazy val line = input.slice(0,offset+1).count(_ == '\n') + 1
        lazy val col = input.slice(0,offset+1).lastIndexOf('\n') match {
            case -1 => offset + 1
            case lineStart => offset - lineStart
        }
    }
    def errorLocation(e: ParseError): Location = e.stack.head._1
    def errorMessage(e: ParseError): String = e.stack.map(_._2).reduce(_ + " "+ _)

    case class ParseError(stack: List[(Location,String)])



    trait Parsers { self =>
        def run[A](p: Parser[A])(input: String): Either[ParseError,A] = {
            val parseResult1 = p(input)
            val (parseResult, error) = parseResult1
            parseResult match {
                case Some((a, "")) => Right(a)
                case _ => Left(error)
            }
        }

        def char(c: Char): Parser[Char] =
            string(c.toString).map(_.charAt(0))
        def number:Parser[List[Int]] = "0-9".r.map(_.toInt).many
        def or[A](s1: Parser[A], s2: Parser[A]): Parser[A]
        implicit def string(s: String): Parser[String] = {
            (x:String) => {
                val error = ParseError(List((Location(x,0), "")))
                val evaluation = if (x.startsWith(s)) Some((s, x.drop(s.length))) else None
                (evaluation, error)

            }
        }
        implicit def regex(r:Regex):Parser[String]
//        implicit def operators[A](p: Parser[A]): ParserOps[A] = ParserOps[A](p)
        implicit def asStringParser[A](a: A)(implicit f: A => Parser[String]):ParserOps[String] = ParserOps(f(a))

        implicit class ParserOps[A](p: Parser[A]) {
            def **[B](p2:Parser[B]):Parser[(A,B)] = self.product(p,p2)
            def product[B](p2:Parser[B]):Parser[(A,B)] = self.product(p,p2)
            def map[B](f:A=>B):Parser[B] = self.map(p)(f)
            def flatMap[B](f:A=>Parser[B]): Parser[B] = self.flatMap(p)(f)
            def |[B>:A](p2: Parser[B]): Parser[B] = self.or(p,p2)
            def or[B>:A](p2: => Parser[B]): Parser[B] = self.or(p,p2)
            def many: Parser[List[A]] = self.many(p)
            def many1: Parser[List[A]] = self.many1(p)
            def slice: Parser[String] = self.slice(p)
        }

        def slice[A](p: Parser[A]): Parser[String]

        def many[A](p:Parser[A]):Parser[List[A]] = map2(p, many(p))(_ :: _).or(succeed(List.empty[A]))

        def many1[A](p:Parser[A]):Parser[List[A]] = product(p, many(p)).map(x=>x._2)

        def map[A,B](p: Parser[A])(f: A => B): Parser[B] = p.flatMap(a=>succeed(f(a)))

        def flatMap[A,B](p: Parser[A])(f: A => Parser[B]): Parser[B] = {
            x:String=>{
                val (firstEvaluation, firstError) = p(x)
                ???
            }
        }

        def succeed[A](a: A): Parser[A] =
            map(string(""))(_ => a)

        def product[A,B](p: Parser[A], p2: => Parser[B]): Parser[(A,B)] = p.flatMap(a=> p2.flatMap(b=>succeed((a,b))))

        def map2[A,B,C](p: Parser[A], p2: =>Parser[B])(f: (A,B) => C): Parser[C] = p**p2 map f.tupled






//            val JArrayParser = ???
            ???

            //        val keyValueItem =







        }

    trait JSON
    object JSON {
        case object JNull extends JSON
        case class JNumber(get: Double) extends JSON
        case class JString(get: String) extends JSON
        case class JBool(get: Boolean) extends JSON
        case class JArray(get: IndexedSeq[JSON]) extends JSON
        case class JObject(get: Map[String, JSON]) extends JSON
        def jsonParser(P: Parsers): Parser[JSON] = {
            import P._
            val spaces = char(' ').many.slice


            def listWithSeparator[T](p: Parser[T], sep: String): Parser[List[T]] = {
                val repetetiveItems = product(string(sep), p).many.map(x => x.map(_._2))
                val nonEmptyList = (p ** repetetiveItems).map(x => x._1 :: x._2)
                nonEmptyList | succeed(List.empty[T])
            }

            val digits = "[0-9]".r.map(_.toInt).many1
            val integerParser = digits.map(_.foldLeft(0)(10 * _ + _))
            val doubleParser = (integerParser ** "." ** digits.map(x => x.zipWithIndex.map(el => el._1 * math.pow(0.1, el._2 + 1)).sum)).map(x => x._1._1 + x._2)
            val standardSymbols = regex("(\\w|\\s)+".r)
            val key = ("\"" ** standardSymbols ** "\"").map(_._1._2)
            def JSONItem: Parser[(String, JSON)] = (key ** ":" ** spaces ** orderedJSONMap(x => x) ** "," ** spaces).map(x => (x._1._1._1._1._1, x._1._1._2))
            def JNullParse: Parser[JSON.JNull.type] = spaces.flatMap(_ => succeed(JNull))
            def JNumberParser: Parser[JNumber] = (doubleParser | integerParser.map(_.toDouble)).map(x => JNumber(x))
            def JStringParser: Parser[JString] = key.map(JString)
            def JBoolParser: Parser[JBool] = or(string("true").map(_ => true), string("false").map(_ => false)).map(JBool)
            def JArrayParser: Parser[JArray] = ("[ " ** orderedJSONMap(listWithSeparator(_, ", ")) ** " ]").map(_._1._2.toIndexedSeq).map(JArray)
            def JSONObjectParser: Parser[JObject] = ("{" ** spaces ** JSONItem.many ** "}").map(x => x._1._2).map(_.toMap).map(JObject)
            def orderedJSONMap[A](f: Parser[JSON] => Parser[A]): Parser[A] = {
                val parsersUnited:List[Parser[JSON]] = List(JSONObjectParser, JArrayParser, JBoolParser, JNumberParser, JStringParser, JNullParse)
                parsersUnited.map(f).reduce((x, y) => x | y)
            }
            JSONObjectParser
        }
    }












}
