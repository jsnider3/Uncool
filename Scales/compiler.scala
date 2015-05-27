import util.parsing.combinator._

object OP extends Enumeration {
  type OP = Value
  val VOID, PLUS, MINUS, MULT, DIV, CMP, NOT,
      NE, GT, GE, LT, LE, EQ = Value
}
import OP._

object Type extends Enumeration {
  type Type = Value
  val INT, ARR, STR, BOOL, OBJ, USR_T = Value
}
import Type._

abstract class Expr

case class UnaOp(op: OP.Value, x: Expr) extends Expr
case class OpExpr(op: OP.Value, x: Expr, y: Expr) extends Expr
case class Constant(ty: Type, con: String) extends Expr
case class While(grd: Expr, bod: Expr) extends Expr
case class LetX(lets: Array[Expr], bod: Expr) extends Expr
case class Seq(bod: Array[Expr]) extends Expr
case class If(gd: Expr, then: Expr, els: Expr) extends Expr
case class Ident(id: String) extends Expr
case class Asgn(id: Expr, rval: Expr) extends Expr
case class ArrAsgn(id: Expr, ind: Expr, rval: Expr) extends Expr
case class MethodCall(id: Expr, args: Array[Expr]) extends Expr
case class ClassCall(self: Expr, id: Expr, args: Array[Expr]) extends Expr

class Cls {

  def getSuper() = "TODO"

}

class Feature {

  def getSuper() = "TODO"

}

class Formal {

  def getSuper() = "TODO"

}

class Let {

  def getSuper() = "TODO"

}

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

  //Var stuff

  def ident:PackratParser[Expr] = """[a-z][A-Za-z0-9_]*""".r  ^^ {
    s => Ident(s)
  }

  def asgn:PackratParser[Expr] = ident ~ "<-" ~ expr  ^^ {
    case a ~ _ ~ b => Asgn(a, b)
  }

  def arrasgn:PackratParser[Expr] = ident ~ "[" ~ expr ~ "]" ~ "<-" ~ expr  ^^ {
    case a ~ _ ~ b ~ _ ~ _ ~ c => ArrAsgn(a, b, c)
  }

  def parens:PackratParser[Expr] = "(" ~ expr ~ ")" ^^ {
    case (_ ~ b ~ _) => b
  }

  //Methods and objects

  def call:PackratParser[Expr] = ident ~ "(" ~ exprlist ~ ")" ^^ {
    case (a ~ _ ~ b ~ _) => MethodCall(a, Array(a))
  }

  def classcall:PackratParser[Expr] = expr ~ "." ~ ident ~ "(" ~ exprlist ~ ")" ^^ {
    case a ~ _ ~ b ~ _ ~ c ~ _ => ClassCall(a, b, Array(b))
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

  //Control structures
  lazy val ifelse: PackratParser[Expr] = (
    "if" ~ expr ~ "then" ~ expr ~ "else" ~ expr ~ "fi") ^^ {
    case _ ~ a ~ _ ~ b ~ _ ~ c ~ _ => If(a, b, c) 
  }

  lazy val let: PackratParser[Let] = (
    ident ~ ":" ~ typename ~ "<-" ~ expr |
    ident ~ ":" ~ typename) ^^ {
    s => new Let
  }

  lazy val letlist: PackratParser[Unit] = (
    let ~ "," ~ letlist | let) ^^ {
    s => 
  }

  lazy val lets: PackratParser[Expr] = (
    "let" ~ letlist ~ "in" ~ expr ~ "tel") ^^ {
    s => LetX(Array(Constant(INT, "0")), Constant(INT, "0"))
  }

  lazy val exprlist: PackratParser[Unit] = (
    expr ~ "," ~ exprlist | expr) ^^ {
    s => 
  }

  lazy val exprseq: PackratParser[Unit] = (
    expr ~ ";" ~ exprseq | expr) ^^ {
    s => 
  }

  lazy val seq: PackratParser[Expr] = (
    "{" ~ exprseq ~ "}") ^^ {
    s => Seq(Array(Constant(INT, "0")))
  }

  lazy val loop: PackratParser[Expr] = (
    "while" ~ expr ~ "loop" ~ expr ~ "pool") ^^ {
    case(_ ~ g ~ _ ~ b ~ _) => While(g, b)
  }

  lazy val expr: PackratParser[Expr] = (
    add | div | mult | sub |
    not | les | leq | neq | gre | geq |eql | isvoid |
    call | classcall | 
    integer | bool | parens | str_const |
    lets | loop | seq | ifelse | asgn | arrasgn | ident 
  )

  lazy val typename: PackratParser[String] = """([A-Z][A-Za-z0-9_]*)""".r ^^ {
    s => s
  }

  lazy val formal: PackratParser[Formal] = (
    ident ~ ":" ~ typename |
    ident ~ ":" ~ "Int" ~ "[" ~ "]") ^^ {
    s => new Formal
  }

  lazy val formals: PackratParser[Array[_ >: Formal]] = (
    formal ~ "," ~ formals |
    formal) ^^ {
    case a ~ _ ~ b => Array(a) ++ Array(b) 
    case a => Array(a)
  }
    
  lazy val feature: PackratParser[Feature] = (
    ident ~ "(" ~ formals ~ ")" ~ ":" ~ typename ~ "{" ~ expr ~ "}" |
    ident ~ "(" ~ ")" ~ ":" ~ typename ~ "{" ~ expr ~ "}" |
    ident ~ ":" ~ "Int" ~ "[" ~ "]" |
    ident ~ ":" ~ typename) ^^ {
    s => new Feature
  }

  lazy val features: PackratParser[Array[_ >: Feature]] = (
    feature ~ ";" ~ features| 
    feature ~ ";") ^^ 
  {
    case a ~ _ ~ b => Array(a) ++ Array(b) 
    case a ~ _ => Array(a)
  }

  lazy val cls: PackratParser[Cls] = 
    "class" ~ typename ~ "{" ~ features ~ "}" ^^ {
    a => new Cls
  }

  lazy val prog: PackratParser[Array[Cls]] = cls  ^^ {
    case c => Array(c)
  }

}

object Compiler extends Comp {
  def main(args: Array[String]) = {
    assert(args.length > 0)
    val sauce = io.Source.fromFile(args(0)).mkString
    println(parseAll(prog, sauce))
  }
}
