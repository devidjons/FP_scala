

object Printable extends App{
    // Define a very simple JSON AST
    sealed trait Json
    final case class JsObject(get: Map[String, Json]) extends Json
    final case class JsString(get: String) extends Json
    final case class JsNumber(get: Double) extends Json
    final case object JsNull extends Json
    // The "serialize to JSON" behaviour is encoded in this trait
    trait JsonWriter[A] {
        def write(value: A): Json
    }

    final case class Person(name: String, email: String)
    object JsonWriterInstances {
        implicit val stringWriter: JsonWriter[String] =
            new JsonWriter[String] {
                def write(value: String): Json =
                    JsString(value)
            }
        implicit val personWriter: JsonWriter[Person] =
            new JsonWriter[Person] {
                def write(value: Person): Json =
                    JsObject(Map(
                        "name" -> JsString(value.name),
                        "email" -> JsString(value.email)
                    ))
            }
    }

    implicit class jsonMethod[A](value:A) {
        def toJson(implicit writer:JsonWriter[A]) :Json = {
            writer.write(value)
        }
    }
//    implicit def optionWriter[A](implicit writer: JsonWriter[A]):JsonWriter[Option[A]] =
//        new JsonWriter[Option[A]] {
//            override def write(value: Option[A]): Json =
//                value match {
//                    case Some(aVal) => writer.write(aVal)
//                    case Option.empty[A] => JsNull
//                }
//        }

//    implicit def optionWriter[A](implicit writer: JsonWriter[A]): JsonWriter[Option[A]] =
//        new JsonWriter[Option[A]] {
//            def write(option: Option[A]): Json =
//                option match {
//                    case Some(aValue) => writer.write(aValue)
//                    case None => JsNull
//                }
//        }

    val a:MyOption[Int] = Some(3)



    import JsonWriterInstances._
    Person("Dave", "dave@example.com").toJson




}
