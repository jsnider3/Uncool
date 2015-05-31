package exprs

object OP extends Enumeration {
  type OP = Value
  val VOID, PLUS, MINUS, MULT, DIV, CMP, NOT,
      NE, GT, GE, LT, LE, EQ = Value
}
import OP._

abstract class Expr

case class UnaOp(op: OP.Value, x: Expr) extends Expr
case class OpExpr(op: OP.Value, x: Expr, y: Expr) extends Expr
case class Constant(ty: String, con: String) extends Expr
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

abstract class Let

case class LetPln(name: String, ty: String) extends Let
case class LetAsgn(name: String, ty:String, body: Expr) extends Let

abstract class Feature

case class Attribute(name: String, ty: String) extends Feature
case class Method(name: String, args:List[Attribute], ty:String, body: Expr) extends Feature


