package fpinscala.parsing

/*
def jsonParser[Err, Parser[+_]](P: Parsers[Err, Parser]): Parser[JSON] = {
  import P._
  val spaces = char(' ').many.slice
}
*/

// List 9-3
trait JSON

object JSON {
  case object JNULL extends JSON
  case class JNumber(get: Double) extends JSON
  case class JString(get: String) extends JSON
  case class JBool(get: Boolean) extends JSON
  case class JArray(get: IndexedSeq[JSON]) extends JSON
  case class jObject(get: Map[String, JSON]) extends JSON
}
