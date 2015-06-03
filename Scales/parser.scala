package scales
import util.parsing.combinator._
import exprs._

class Comp extends RegexParsers with PackratParsers {

  //Constants

  lazy val integer:PackratParser[Expr] = """-?\d+""".r ^^ {
    s => Constant("Int", s)
  }

  def bool:PackratParser[Expr] = "(true)|(false)".r  ^^ {
    s => Constant("Bool", s)
  }

  def str_const:PackratParser[Expr] = "\"[^\"]*\"".r  ^^ {
    s => Constant("String", s)
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
    case(a ~ "+" ~ b) => OpExpr(OP.PLUS, a, b)
  }

  lazy val div: PackratParser[Expr] = expr ~ "/" ~ expr ^^ {
    case(a ~ "/" ~ b) => OpExpr(OP.DIV, a, b)
  }

  lazy val mult: PackratParser[Expr] = expr ~ "*" ~ expr ^^ {
    case(a ~ "*" ~ b) => OpExpr(OP.MULT, a, b)
  }

  lazy val sub: PackratParser[Expr] = expr ~ "-" ~ expr ^^ {
    case(a ~ "-" ~ b) => OpExpr(OP.MINUS, a, b)
  }

  //Booleans

  lazy val cmp: PackratParser[Expr] = "~" ~> expr ^^ {
    a => UnaOp(OP.CMP, a)
  }

  lazy val not: PackratParser[Expr] = "not" ~> expr ^^ {
    a => UnaOp(OP.NOT, a)
  }

  lazy val isvoid: PackratParser[Expr] = "isvoid" ~> expr ^^ {
    a => UnaOp(OP.VOID, a)
  }

  lazy val les: PackratParser[Expr] = (expr <~ "<") ~ expr ^^ {
    case(a ~ b) => OpExpr(OP.LT, a, b)
  }

  lazy val leq: PackratParser[Expr] = (expr <~ "<=") ~ expr ^^ {
    case(a ~ b) => OpExpr(OP.LE, a, b)
  }

  lazy val neq: PackratParser[Expr] = (expr <~ "<>") ~ expr ^^ {
    case(a ~ b) => OpExpr(OP.NE, a, b)
  }

  lazy val gre: PackratParser[Expr] = (expr <~ ">") ~ expr ^^ {
    case(a ~ b) => OpExpr(OP.GT, a, b)
  }

  lazy val geq: PackratParser[Expr] = (expr <~ ">=") ~ expr ^^ {
    case(a ~ b) => OpExpr(OP.GE, a, b)
  }

  lazy val eql: PackratParser[Expr] = (expr <~ "=") ~ expr ^^ {
    case(a ~ b) => OpExpr(OP.EQ, a, b)
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
  def make_ast(file: String) : List[Cls]= {
    val sauce = io.Source.fromFile(file).mkString
    return parseAll(prog, sauce).get
  }
}
