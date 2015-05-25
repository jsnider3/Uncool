import scala.util.parsing.combinator._

object OP extends Enumeration {
  type OP = Value
  val VOID, PLUS, MINUS, MULT, DIV, CMP, NOT,
      NE, GT, GE, LT, LE, EQ = Value
}
import OP._

object Type extends Enumeration {
  type Type = Value
  val INT, ARR, STR, BOOL, OBJ, UNK = Value
}
import Type._

abstract class Expr

case class UnaOp(op: OP.Value, x: Expr) extends Expr
case class OpExpr(op: OP.Value, x: Expr, y: Expr) extends Expr
case class Add(x: Expr, y: Expr) extends Expr
case class Constant(ty: Type, con: String) extends Expr

class Comp extends RegexParsers with PackratParsers {

  //Constants

  lazy val integer:PackratParser[Expr] = """-?\d+""".r ^^ {
    s => Constant(INT, s)
  }

  def bool:PackratParser[Expr] = """(true)|(false)""".r  ^^ {
    s => Constant(BOOL, s)
  }

  def str_const:PackratParser[Expr] = """\"[^\"]*\"""".r  ^^ {
    s => Constant(STR, s)
  }

  def ident:PackratParser[Expr] = """(true)|(false)""".r  ^^ {
    s => Constant(BOOL, s)
  }

  def parens:PackratParser[Expr] = "(" ~ expr ~ ")" ^^ {
    case (a ~ b ~ c) => b
  }

  // Arithmetic

  lazy val add: PackratParser[Expr] = expr ~ "+" ~ expr ^^ {
    case(a ~ "+" ~ b) => OpExpr(PLUS, a, b)
  }

  lazy val div: PackratParser[Expr] = expr ~ "/" ~ expr ^^ {
    case(a ~ "/" ~ b) => OpExpr(DIV, a, b)
  }

  lazy val mult: PackratParser[Expr] = expr ~ "*" ~ expr ^^ {
    case(a ~ "*" ~ b) => OpExpr(MULT, a, b)
  }

  lazy val sub: PackratParser[Expr] = expr ~ "-" ~ expr ^^ {
    case(a ~ "-" ~ b) => OpExpr(MINUS, a, b)
  }

  //Booleans

  lazy val cmp: PackratParser[Expr] = "~" ~ expr ^^ {
    case("~" ~ a) => UnaOp(CMP, a)
  }

  lazy val not: PackratParser[Expr] = "not" ~ expr ^^ {
    case("not" ~ a) => UnaOp(NOT, a)
  }

  lazy val isvoid: PackratParser[Expr] = "isvoid" ~ expr ^^ {
    case("isvoid" ~ a) => UnaOp(VOID, a)
  }

  lazy val les: PackratParser[Expr] = expr ~ "<" ~ expr ^^ {
    case(a ~ "<" ~ b) => OpExpr(LT, a, b)
  }

  lazy val leq: PackratParser[Expr] = expr ~ "<=" ~ expr ^^ {
    case(a ~ "<=" ~ b) => OpExpr(LE, a, b)
  }

  lazy val neq: PackratParser[Expr] = expr ~ "<>" ~ expr ^^ {
    case(a ~ "<>" ~ b) => OpExpr(NE, a, b)
  }

  lazy val gre: PackratParser[Expr] = expr ~ ">" ~ expr ^^ {
    case(a ~ ">" ~ b) => OpExpr(GT, a, b)
  }

  lazy val geq: PackratParser[Expr] = expr ~ ">=" ~ expr ^^ {
    case(a ~ ">=" ~ b) => OpExpr(GE, a, b)
  }

  lazy val eql: PackratParser[Expr] = expr ~ "=" ~ expr ^^ {
    case(a ~ "=" ~ b) => OpExpr(EQ, a, b)
  }

  lazy val expr: PackratParser[Expr] = (
    add | div | mult | sub |
    integer | bool | parens |
    not | les | leq | neq | gre | geq |eql | isvoid 
  )

}

object Compiler extends Comp {
  def main(args: Array[String]) = println(parseAll(expr, "5+ -3"))
}
