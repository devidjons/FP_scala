import Parsing.JSON.JNull
import Parsing._

import scala.util.matching.Regex

object Parsing {


    trait Parsers[ParseError, Parser[+_]] { self =>
        def char(c: Char): Parser[Char] =
            map(string(c.toString))(_.charAt(0))
        def number:Parser[List[Int]] = "0-9".r.map(_.toInt).many
        def or[A](s1: Parser[A], s2: Parser[A]): Parser[A]
        implicit def string(s: String): Parser[String]
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

        def flatMap[A,B](p: Parser[A])(f: A => Parser[B]): Parser[B]

        def succeed[A](a: A): Parser[A] =
            map(string(""))(_ => a)

        def product[A,B](p: Parser[A], p2: => Parser[B]): Parser[(A,B)] = p.flatMap(a=> p2.flatMap(b=>succeed((a,b))))

        def map2[A,B,C](p: Parser[A], p2: =>Parser[B])(f: (A,B) => C): Parser[C] = p**p2 map f.tupled

        def jsonParser[Err,Parser[+_]](P: Parsers[Err,Parser]): Parser[JSON] = {
//            import P._
            import JSON._
            val spaces = char(' ').many
            val digits = "[0-9]".r.map(_.toInt).many1
            val integerParser = digits.map(_.foldLeft(0)(10*_+_))
            val doubleParser = (integerParser ** "." ** digits.map(x=> x.zipWithIndex.map(el => el._1 * math.pow(0.1, el._2 + 1)).sum)).map(x=> x._1._1 + x._2)
            val standardSymbols = regex("(\\w|\\s)+".r)
            val key = ("\"" **  standardSymbols ** "\"").map(_._1._2)
            val JNullParse = spaces.flatMap(_=> succeed(JNull))
            val JNumberParser = (doubleParser | integerParser.map(_.toDouble)).map(x=>JNumber(x))
            val JStringParser = key.map(JString(_))
            val JBoolParser = or(string("true").map(_ => true), string("false").map(_=> false))
            val JSONParser:Parser[JSON] = ???
            val JArrayParser = ???
            ???

            //        val keyValueItem =
        }






        }

    trait JSON
    object JSON {
        case object JNull extends JSON
        case class JNumber(get: Double) extends JSON
        case class JString(get: String) extends JSON
        case class JBool(get: Boolean) extends JSON
        case class JArray(get: IndexedSeq[JSON]) extends JSON
        case class JObject(get: Map[String, JSON]) extends JSON
    }












}
