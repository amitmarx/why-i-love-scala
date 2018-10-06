
import scala.util.Try

case class Plus(a: Int, b: Int)
case class Minus(a: Int, b: Int)
case class Unparsed(text: String)

type Params = Seq[String]
type Extractor[A] = PartialFunction[(String, Params), A]

val readPlus: Extractor[Plus] = {
  case ("+", AllInts(a, b)) => Plus(a, b)
}

val readMinus: Extractor[Minus] = {
  case ("-", AllInts(a, b)) => Minus(a, b)
}

class TextParser(extractors: Extractor[_]*) {
  private val defaultFunction: Extractor[Any] = {
    case (operator, params) => Unparsed(operator + ":" + params.mkString(" "))
  }

  private val extractorFunction: Extractor[Any] = extractors.foldRight(defaultFunction)(_ orElse _)

  def parse(txt: String) =
    txt.split(":").toList match {
      case head :: Nil => Unparsed(txt)
      case Nil => Unparsed(txt)
      case head :: tail =>
        val params = tail.mkString(":").split(" ")
        extractorFunction(head, params)
      case _ =>
    }
}

val parser = new TextParser(readMinus, readPlus)
parser.parse("+:1 2 3")
parser.parse("-:1 2 3")
parser.parse("-:1")
parser.parse("-1")
parser.parse("")


object AllInts{
  def tryInt(str: String): Option[Int] = Try(str.toInt).toOption
  def unapply(arg: Seq[String]): Option[(Int, Int)] ={
    val arg1 = arg.headOption.flatMap(tryInt)
    val arg2 = arg.tail.headOption.flatMap(tryInt)
    arg2.flatMap(x1 => arg1.map(x2 => (x1, x2)))
  }
}
