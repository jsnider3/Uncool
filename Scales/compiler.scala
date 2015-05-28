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
case class ArrGet(id: Expr, ind: Expr) extends Expr
case class MethodCall(id: Expr, args: Array[Expr]) extends Expr
case class ClassCall(self: Expr, id: Expr, args: Array[Expr]) extends Expr

class Cls (name: String, parent: String, feats: Array[Feature]){

  def getSuper() = "TODO"

}

class Feature {

  def getSuper() = "TODO"

}

class Formal (name: String, ty: String){

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

  def bool:PackratParser[Expr] = "(true)|(false)".r  ^^ {
    s => Constant(BOOL, s)
  }

  def str_const:PackratParser[Expr] = "\"[^\"]*\"".r  ^^ {
    s => Constant(STR, s)
  }

  //Var stuff
  def varname:PackratParser[String] = "[a-z][A-Za-z0-9_]*".r  ^^ {
    s => s
  }

  def ident:PackratParser[Expr] = varname  ^^ {
    s => Ident(s)
  }

  def asgn:PackratParser[Expr] = ident ~ "<-" ~ expr  ^^ {
    case a ~ _ ~ b => Asgn(a, b)
  }

  def arrget:PackratParser[Expr] = ident ~ "[" ~ expr <~ "]" ^^ {
    case a ~ _ ~ b => ArrGet(a, b)
  }

  def arrasgn:PackratParser[Expr] = ident ~ "[" ~ expr ~ "]" ~ "<-" ~ expr  ^^ {
    case a ~ _ ~ b ~ _ ~ _ ~ c => ArrAsgn(a, b, c)
  }

  def parens:PackratParser[Expr] = "(" ~> expr <~ ")" 

  //Methods and objects

  def call:PackratParser[Expr] = ident ~ "(" ~ exprlist <~ ")" ^^ {
    case (a ~ _ ~ b) => MethodCall(a, Array(a))
  }

  def classcall:PackratParser[Expr] = expr ~ "." ~ ident ~ "(" ~ exprlist <~ ")" ^^ {
    case a ~ _ ~ b ~ _ ~ c => ClassCall(a, b, c)
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

  lazy val cmp: PackratParser[Expr] = "~" ~> expr ^^ {
    a => UnaOp(CMP, a)
  }

  lazy val not: PackratParser[Expr] = "not" ~> expr ^^ {
    a => UnaOp(NOT, a)
  }

  lazy val isvoid: PackratParser[Expr] = "isvoid" ~> expr ^^ {
    a => UnaOp(VOID, a)
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
    "if" ~> expr ~ "then" ~ expr ~ "else" ~ expr <~ "fi") ^^ {
    case a ~ _ ~ b ~ _ ~ c => If(a, b, c) 
  }

  lazy val let: PackratParser[Let] = (
    ident ~ ":" ~ typename ~ "<-" ~ expr |
    ident ~ ":" ~ typename) ^^ {
    s => new Let
  }

  lazy val letlist: PackratParser[Array[Let]] = repsep(let, ",") ^^ {
    _.toArray
  }

  lazy val lets: PackratParser[Expr] = (
    "let" ~> letlist ~ "in" ~ expr <~ "tel") ^^ {
    s => LetX(Array(Constant(INT, "0")), Constant(INT, "0"))
  }

  lazy val exprlist: PackratParser[Array[Expr]] = repsep(expr, ",") ^^ {
    _.toArray 
  }

  lazy val seq: PackratParser[Expr] = "{" ~> repsep(expr, ";") <~ "}" ^^ {
    s => Seq(s.toArray)
  }

  lazy val loop: PackratParser[Expr] = (
    "while" ~> expr ~ "loop" ~ expr <~ "pool") ^^ {
    case(g ~ _ ~ b) => While(g, b)
  }

  lazy val expr: PackratParser[Expr] = (
    add | div | mult | sub |
    not | les | leq | neq | gre | geq |eql | isvoid |
    call | classcall | 
    integer | bool | parens | str_const |
    lets | loop | seq | ifelse | 
    asgn | arrasgn | arrget | ident 
  )

  lazy val typename: PackratParser[String] = "([A-Z][A-Za-z0-9_]*)".r ^^ {
    s => s
  } 

  lazy val formal: PackratParser[Formal] = (
    varname ~ (":" ~> "Int" ~> "[" ~> "]") |
    varname ~ (":" ~> typename)) ^^ {
    case i ~ (t : String) => new Formal(i, t)
  }

  lazy val formals: PackratParser[Array[Formal]] = repsep(formal, ",") ^^ {
    _.toArray
  }
    
  lazy val feature: PackratParser[Feature] = (
    ident ~ "(" ~ formals ~ ")" ~ ":" ~ typename ~ "{" ~ expr <~ "}" |
    ident ~ ":" ~ "Int" ~ "[" ~ "]" |
    ident ~ ":" ~ typename) ^^ {
    s => new Feature
  }

  lazy val features: PackratParser[Array[Feature]] = rep(feature <~ ";") ^^ {
    _.toArray
  }

  lazy val cls: PackratParser[Cls] = 
    "class" ~> typename ~ "{" ~ features <~ "}" ^^ {
    case a ~ _ ~ b => new Cls(a, "Object", b)
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
