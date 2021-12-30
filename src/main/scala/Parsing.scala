import Parsing.JSON.{JNull, jsonParser}

import Parsing._

import scala.::
import scala.Option
import scala.util.matching.Regex

object Parsing {
    case class Location(input: String, offset: Int = 0) {
        lazy val line = input.slice(0,offset+1).count(_ == '\n') + 1
        lazy val col = input.slice(0,offset+1).lastIndexOf('\n') match {
            case -1 => offset + 1
            case lineStart => offset - lineStart
        }
        def advanceTo(n: Int): Location =
            copy(offset = n)
    }
    def errorLocation(e: ParseError): Location = ???
    def errorMessage(e: ParseError): String = ???

    case class ParseError(stack: List[(Location,String)] = List.empty) {
        def push(loc: Location, msg: String): ParseError =
            copy(stack = (loc,msg) :: stack)

        def label[A](s: String): ParseError = {
            val r1 =latestLoc.map(x=> (x,s) :: List.empty).getOrElse(List.empty)
            ParseError(r1)
        }

        def latestLoc: Option[Location] =
            latest map (_._1)
        def latest: Option[(Location, String)] =
            stack.lastOption
    }



    trait Parsers[Parser[+_]] { self =>
        def run[A](p: Parser[A])(input: String): Either[ParseError,A]

        def char(c: Char): Parser[Char] =
            string(c.toString).map(_.charAt(0))
        def number:Parser[List[Int]] = "0-9".r.map(_.toInt).many1
        def or[A](s1: Parser[A], s2: =>Parser[A]): Parser[A]
        implicit def string(s: String): Parser[String]
        implicit def regex(r:Regex):Parser[String]
//        implicit def operators[A](p: Parser[A]): ParserOps[A] = ParserOps[A](p)
        implicit def asStringParser[A](a: A)(implicit f: A => Parser[String]):ParserOps[String] = ParserOps(f(a))

        implicit class ParserOps[A](p: Parser[A]) {
            def **[B](p2: =>Parser[B]):Parser[(A,B)] = self.product(p,p2)
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

        def many[A](p:Parser[A]):Parser[List[A]] =map2(p, many(p))(_ :: _).or(succeed(List.empty[A]))


        def many1[A](p:Parser[A]):Parser[List[A]] = product(p, many(p)).map(x=>x._1 :: x._2)

        def map[A,B](p: Parser[A])(f: A => B): Parser[B] = p.flatMap(a=>succeed(f(a)))

        def flatMap[A,B](p: Parser[A])(f: A => Parser[B]): Parser[B]

        def succeed[A](a: A): Parser[A] //= map(string(""))(_ => a)

        def product[A,B](p: Parser[A], p2: => Parser[B]): Parser[(A,B)] = p.flatMap(a=> p2.flatMap(b=>succeed((a,b))))

        def map2[A,B,C](p: Parser[A], p2: =>Parser[B])(f: (A,B) => C): Parser[C] = p**p2 map f.tupled


    }

    trait JSON
    object JSON {
        case object JNull extends JSON
        case class JNumber(get: Double) extends JSON
        case class JString(get: String) extends JSON
        case class JBool(get: Boolean) extends JSON
        case class JArray(get: IndexedSeq[JSON]) extends JSON
        case class JObject(get: Map[String, JSON]) extends JSON
        def jsonParser[Parser[+_]](P: Parsers[Parser]): Parser[JSON] = {
            import P._
            val spaces = char(' ').many


            def listWithSeparator[T](p: Parser[T], sep: String): Parser[List[T]] = {
                val repetetiveItems = product(string(sep), p).many.map(x => x.map(_._2))
                val nonEmptyList = (p ** repetetiveItems).map(x => x._1 :: x._2)
                nonEmptyList | succeed(List.empty[T])
            }

            val digits = 0.to(9).map(x=> string(x.toString)).reduce(_ | _).map(_.toInt).many1
            val integerParser = digits.map(_.foldLeft(0)(10 * _ + _))

            val doubleParser = (integerParser ** "." ** digits.map(x => x.map(_.toLong).zipWithIndex.map(el => el._1 * math.pow(0.1, el._2 + 1)).sum)).map(x =>{println(x);x._1._1 + x._2})
            val standardSymbols = regex("(\\w|\\s)+".r)
            val key = ("\"" ** standardSymbols ** "\"").map(_._1._2)
            def JSONItem: Parser[(String, JSON)] = (key ** ":" ** spaces ** orderedJSONMapLst(x => x) ** (",".or("")) ** spaces).map(x => (x._1._1._1._1._1, x._1._1._2))
            def JNullParse: Parser[JSON.JNull.type] = spaces.flatMap(_ => succeed(JNull))
            def JNumberParser: Parser[JNumber] = (doubleParser | integerParser.map(_.toDouble)).map(x => JNumber(x))
            def JStringParser: Parser[JString] = key.map(JString)
            def JBoolParser: Parser[JBool] = or(string("true").map(_ => true), string("false").map(_ => false)).map(JBool)
            def JArrayParser: Parser[JArray] = ("[ " ** orderedJSONMapEl(listWithSeparator(_, ", ")) ** " ]").map(_._1._2.toIndexedSeq).map(JArray)
            def JSONObjectParser: Parser[JObject] = ("{" ** spaces ** JSONItem.many1 ** "}").map(x => x._1._2).map(_.toMap).map(JObject)
            def orderedJSONMapLst[A](f: Parser[JSON] => Parser[A]): Parser[A] = {
                val parsersUnited:Stream[Parser[JSON]] = Stream(JBoolParser, JNumberParser, JStringParser, JNullParse, JArrayParser, JSONObjectParser)
                parsersUnited.map(f).reduce((x, y) => x | y)
            }
            def orderedJSONMapEl[A](f: Parser[JSON] => Parser[A]): Parser[A] = {
                val parsersUnited:Stream[Parser[JSON]] = Stream(JBoolParser, JNumberParser, JStringParser, JNullParse, JArrayParser, JSONObjectParser)
                parsersUnited.map(f).reduce((x, y) => x | y)
            }
            JSONObjectParser
        }
    }

    trait Result[+A] {
        def mapError(f: ParseError => ParseError): Result[A] = this match {
            case Failure(e,c) => Failure(f(e),c)
            case _ => this
        }

        def map[B>:A](f:A=>B):Result[B] = {
            this match {
                case Success(a, count) => Success(f(a), count)
                case Failure(x, c) => Failure(x,c)
            }
        }

        def uncommit: Result[A] = this match {
            case Failure(e,true) => Failure(e,false)
            case _ => this
        }

        def addCommit(isCommitted: Boolean): Result[A] = this match {
            case Failure(e,c) => Failure(e, c || isCommitted)
            case _ => this
        }

        def advanceSuccess(n: Int): Result[A] = this match {
            case Success(a,m) => Success(a,n+m)
            case _ => this
        }




    }
    case class Success[+A](get: A, charsConsumed: Int) extends Result[A]
    case class Failure(get: ParseError, isCommitted: Boolean = true) extends Result[Nothing]


    type MyParser[+A] = Location => Result[A]
    object MyParsers extends Parsers[MyParser] {
        override def run[A](p: MyParser[A])(input: String): Either[ParseError, A] =
            p(Location(input,0)) match {
                case Success(get, _) => Right(get)
                case Failure(err,_)=> Left(err)
            }

        override def or[A](s1: MyParser[A], s2: =>MyParser[A]): MyParser[A] =
            s => attempt(s1)(s) match {
                case Failure(_,false) => s2(s)
                case r => r
            }


        override implicit def regex(r: Regex): MyParser[String] = {
            x:Location  =>{
                val err = ParseError(List((x, r.toString())))
                val input0 = x.input.drop(x.offset)
                r.findFirstIn(input0).map(str => Success(str, x.offset + str.length)).getOrElse(Failure(err))
            }
        }

        override def slice[A](p: MyParser[A]): MyParser[String] = {
            x:Location=>{
                p(x) match {
                    case Success(_, newOffset) => Success(x.input.slice(x.offset, newOffset), newOffset)
                    case Failure(x,c) => Failure(x,c)
                }
            }
        }

        def flatMap[A,B](f: MyParser[A])(g: A => MyParser[B]): MyParser[B] =
            s => f(s) match {
                case Success(a,n) => g(a)(s.advanceTo(n)).addCommit((n-s.offset) != 0)
                case e@Failure(_,_) => e
        }


        override implicit def string(s: String): MyParser[String] = {
            val raw = (x:Location) => {
                if (x.input.drop(x.offset).startsWith(s)) {
                    Success(s, s.length + x.offset)
                } else {
                    Failure(ParseError(List((x, "expected :"+s + " obtainded :" + x.input.drop(x.offset)))))
                }
            }
            raw
        }
        def scope[A](msg: String)(p: MyParser[A]): MyParser[A] =
            s => p(s).mapError(_.push(s,msg))

        def label[A](msg: String)(p: MyParser[A]): MyParser[A] =
            s => p(s).mapError(_.label(msg))

        def attempt[A](p: MyParser[A]): MyParser[A] =
            s => p(s).uncommit

        override def succeed[A](a: A): MyParser[A] =
            s=> Success(a,s.offset)
    }
}

object test11 extends App {
    import MyParsers._
    import Parsing.MyParser
    import MyParsers.ParserOps
    val jsPars = jsonParser[MyParser](MyParsers)
    val input = "{ \"Age\":12 }"
    val input2 = "{\"menu\": 1.023 }"
//    val input = "aaaaaa"
//    val pars= many(string("a"))
//    val pars = succeed("a")
    println(run(jsPars)(input2))
}
