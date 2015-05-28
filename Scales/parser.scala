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
case class LetX(lets: List[Let], bod: Expr) extends Expr
case class Seq(bod: List[Expr]) extends Expr
case class If(gd: Expr, then: Expr, els: Expr) extends Expr
case class Var(id: String) extends Expr
case class Asgn(id: Expr, rval: Expr) extends Expr
case class ArrAsgn(id: Expr, ind: Expr, rval: Expr) extends Expr
case class ArrDec(size: Expr) extends Expr
case class ArrGet(id: Expr, ind: Expr) extends Expr
case class MethodCall(id: Expr, args: List[Expr]) extends Expr
case class ClassCall(self: Expr, id: Expr, args: List[Expr]) extends Expr

class Cls (name: String, parent: String, feats: List[Feature]){

  override def toString() = ("class " + name + " inherits " + parent + " {" +
                              feats.mkString("; ") + "}")

  def getSuper() = parent

}

abstract class Feature

case class Attribute(name: String, ty: String) extends Feature
case class Method(name: String, args:Any, ty:String, body: Expr) extends Feature

abstract class Let

case class LetPln(name: String, ty: String) extends Let
case class LetAsgn(name: String, ty:String, body: Expr) extends Let

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
    s => Var(s)
  }

  def asgn:PackratParser[Expr] = ident ~ ("<-" ~> expr)  ^^ {
    case a ~ b => Asgn(a, b)
  }

  def arrget:PackratParser[Expr] = ident ~ ("[" ~> expr <~ "]") ^^ {
    case a ~ b => ArrGet(a, b)
  }

  def arrdec:PackratParser[Expr] = "new" ~> "Int" ~> "[" ~> expr <~ "]" ^^ {
    a => ArrDec(a)
  }


  def arrasgn:PackratParser[Expr] = ident ~ ("[" ~> expr <~ "]" <~ "<-") ~ expr  ^^ {
    case a ~ b ~ c => ArrAsgn(a, b, c)
  }

  def parens:PackratParser[Expr] = "(" ~> expr <~ ")" 

  //Methods and objects

  def call:PackratParser[Expr] = ident ~ ("(" ~> exprlist <~ ")") ^^ {
    case (a ~ b) => MethodCall(a, b)
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

  lazy val les: PackratParser[Expr] = (expr <~ "<") ~ expr ^^ {
    case(a ~ b) => OpExpr(LT, a, b)
  }

  lazy val leq: PackratParser[Expr] = (expr <~ "<=") ~ expr ^^ {
    case(a ~ b) => OpExpr(LE, a, b)
  }

  lazy val neq: PackratParser[Expr] = (expr <~ "<>") ~ expr ^^ {
    case(a ~ b) => OpExpr(NE, a, b)
  }

  lazy val gre: PackratParser[Expr] = (expr <~ ">") ~ expr ^^ {
    case(a ~ b) => OpExpr(GT, a, b)
  }

  lazy val geq: PackratParser[Expr] = (expr <~ ">=") ~ expr ^^ {
    case(a ~ b) => OpExpr(GE, a, b)
  }

  lazy val eql: PackratParser[Expr] = (expr <~ "=") ~ expr ^^ {
    case(a ~ b) => OpExpr(EQ, a, b)
  }

  //Control structures
  lazy val ifelse: PackratParser[Expr] = (
    "if" ~> expr ~ "then" ~ expr ~ "else" ~ expr <~ "fi") ^^ {
    case a ~ _ ~ b ~ _ ~ c => If(a, b, c) 
  }

  lazy val letasgn: PackratParser[Let] = (
    varname ~ (":" ~> typename <~ "<-") ~ expr) ^^ {
    case i ~ t ~ e => LetAsgn(i, t, e)
  }

  lazy val letpln: PackratParser[Let] = (
    varname ~ (":" ~> typename)) ^^ {
    case i ~ t => LetPln(i, t)
  }

  lazy val letlist: PackratParser[List[Let]] = repsep(letasgn | letpln, ",")

  lazy val lets: PackratParser[Expr] = (
    "let" ~> letlist ~ ("in" ~> expr <~ "tel")) ^^ {
    case ls ~ e => LetX(ls, e)
  }

  lazy val exprlist: PackratParser[List[Expr]] = repsep(expr, ",")

  lazy val seq: PackratParser[Expr] = "{" ~> repsep(expr, ";") <~ "}" ^^ {
    s => Seq(s)
  }

  lazy val loop: PackratParser[Expr] = (
    ("while" ~> expr <~ "loop") ~ expr <~ "pool") ^^ {
    case(g ~ b) => While(g, b)
  }

  lazy val expr: PackratParser[Expr] = (
    add | div | mult | sub |
    not | les | leq | neq | gre | geq |eql | isvoid |
    call | classcall | 
    integer | bool | parens | str_const |
    lets | loop | seq | ifelse | 
    asgn | arrasgn | arrdec | arrget | ident 
  )

  lazy val typename: PackratParser[String] = "([A-Z][A-Za-z0-9_]*)".r ^^ {
    s => s
  } 

  lazy val args: PackratParser[List[Attribute]] = repsep(attribute, ",")

  lazy val method: PackratParser[Method] = (
    varname ~ ("(" ~> args <~ ")" <~ ":") ~ typename ~ ("{" ~> expr <~ "}")) ^^ {
    case s ~ f ~ t ~ e => Method(s, f, t, e)
  }
    
  lazy val attribute: PackratParser[Attribute] = (
    varname ~ (":" ~> "Int" ~> "[" ~> "]") |
    varname ~ (":" ~> typename)) ^^ {
    case s ~ "]" => Attribute(s, "Int[]")
    case s ~ t => Attribute(s, t)
  }

  lazy val features: PackratParser[List[Feature]] = (
    rep((attribute | method) <~ ";"))

  lazy val cls: PackratParser[Cls] = (
    "class" ~> typename ~ ("{" ~> features <~ "}"))  ^^{
    case a ~ b => new Cls(a, "Object", b)
  }

  lazy val subcls: PackratParser[Cls] = (
    "class" ~> typename ~ ("inherits" ~> typename <~ "{") ~ features <~ "}") ^^ {
    case a ~ b ~ c => new Cls(a, b, c)
  }

  lazy val prog: PackratParser[List[Cls]] = rep(cls | subcls)

}

object Uncool extends Comp {
  def main(args: Array[String]) = {
    assert(args.length > 0)
    val sauce = io.Source.fromFile(args(0)).mkString
    println(parseAll(prog, sauce).get.mkString(" "))
  }
}
