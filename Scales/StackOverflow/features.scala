import util.parsing.combinator._

class Element {

  def getSuper() = "TODO"

}

class Comp extends RegexParsers with PackratParsers {
  lazy val element: PackratParser[Element] = (
    "foo") ^^ {
    s => new Element
  }

  lazy val list: PackratParser[Array[_ >: Element]] = (
    (element ~ ";" ~ list) | 
    element ~ ";") ^^ 
  {
    case a ~ ";" ~ b => Array(a) ++ Array(b)
    case a ~ ";" => Array(a)
  }
}

object Compiler extends Comp {
  def main(args: Array[String]) = {
    println(parseAll(list, "foo; foo; foo;"))
  }
}
