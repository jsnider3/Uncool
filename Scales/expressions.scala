package scales.exprs
import scala.collection.mutable.Map
import scales.Main
import scales.Log

object OP extends Enumeration {
  type OP = Value
  val VOID, PLUS, MINUS, MULT, DIV, CMP, NOT,
      NE, GT, GE, LT, LE, EQ = Value
}
import OP._

trait Expr {
  def typecheck(state: Map[String, String]) : String
}

case class UnaOp(op: OP.Value, x: Expr) extends Expr {
  def typecheck(state: Map[String, String]) : String = {
    val ty = x.typecheck(state)
    op match {
      case CMP => if (ty != "Int") {Log.error("Complement of non-int")}
      case NOT => if (ty != "Bool") {Log.error("Negating non-bool")}
    }
    if (op == CMP) {
      "Int"
    } else {
      "Bool"
    }
  }

}

case class OpExpr(op: OP.Value, x: Expr, y: Expr) extends Expr{
  def typecheck(state: Map[String, String]) : String = {
    val tys = Array(x.typecheck(state), y.typecheck(state))
    if (tys(0) != tys(1) || tys(1) != "Int") {
      Log.error(op + " needs two ints.")
    }
    if (op == NE || op == GT || op == GE || op == LT || op == LE || op == EQ) {
      "Bool"
    } else {
      "Int"
    }
  }

}

case class Constant(ty: String, con: String) extends Expr {
  def typecheck(state: Map[String, String]) : String = ty

}

case class While(grd: Expr, bod: Expr) extends Expr {
  def typecheck(state: Map[String, String]) : String = {
    if (grd.typecheck(state) != "Bool") {
      Log.error("While guard " + grd + " non-bool.")
    }
    bod.typecheck(state)
    "Int"
  }

}

case class LetX(lets: List[Let], bod: Expr) extends Expr {
  def typecheck(state: Map[String, String]) : String = {
    var letstate = state.clone()
    lets.foreach{_.typecheck(letstate)}
    lets.foreach{_.load(letstate)}
    bod.typecheck(letstate)
  }

}

case class Seq(bod: List[Expr]) extends Expr {
  def typecheck(state: Map[String, String]) : String = {
    (bod map {_.typecheck(state)}).last
  }

}

case class If(gd: Expr, then: Expr, els: Expr) extends Expr {
  def typecheck(state: Map[String, String]) : String = {
    if (gd.typecheck(state) != "Bool") {
      Log.error("If guard " + gd + " non-bool.")
    }
    val tys = List(then.typecheck(state.clone()), els.typecheck(state.clone()))
    //TODO Common types.
    if (tys(0) != tys(1)) {
      Log.error("If branches of differing types.")
    }
    tys(0)
  }
}

case class Var(id: String) extends Expr {
  def typecheck(state: Map[String, String]) : String = {
    if (!state.contains(id)) {
      Log.error(id + " is not in-scope.")
      "Int"
    } else {
      state(id)
    }
  }
}

case class Asgn(id: Expr, rval: Expr) extends Expr {
  def typecheck(state: Map[String, String]) : String = {
    val tys = List(id.typecheck(state), rval.typecheck(state))
    if (tys(0) != tys(1)) {
      Log.error("Assignment to " + tys(0) + " " + id + " of type " + tys(1))
    }
    tys(1)
  }
}

case class ArrAsgn(id: Expr, ind: Expr, rval: Expr) extends Expr {
  def typecheck(state: Map[String, String]) : String = {
    val tys = List(id.typecheck(state), ind.typecheck(state), rval.typecheck(state))
    if (tys(0) != "Int[]") {
      Log.error("Array assignment to non-array.")
    }
    if (tys(1) != "Int") {
      Log.error("Array index " + ind + " of wrong type.")
    }
    if (tys(2) != "Int") {
      Log.error("Array assignment to " + id + " of type " + tys(2))
    }
    "Int"
  }
}

case class ArrDec(size: Expr) extends Expr {
  def typecheck(state: Map[String, String]) : String = {
    val ty = size.typecheck(state)
    if (ty != "Int") {
      Log.error("New array declared with size of type " + ty + ".")
    }
    "Int[]"
  }
}

case class ArrGet(id: Expr, ind: Expr) extends Expr {
  def typecheck(state: Map[String, String]) : String = {
    if (ind.typecheck(state) != "Int") {
      Log.error("Indexing into array with non-Int.")
    }
    "Int"
  }
}

trait Callable {
  def typecheckCall(meth: Method, args: List[Expr], state: Map[String, String]) = {
    val vals = args map {_.typecheck(state)}
    if (vals.length != meth.args.length) {
      Log.error("Method call on " + meth + " has wrong number of args.")
    }
    val bindings = meth.args zip vals
    bindings.foreach{case (Attribute(n, t), v) => if (v != t) {
      Log.error("Arg of wrong type")
    }}
  }
}

case class MethodCall(id: Expr, args: List[Expr]) extends Expr with Callable {
  def typecheck(state: Map[String, String]) : String = {
    val cls = Main.prog.find({_.Name() == state("self")}).get
    id match {
      case Var(n) => {
        val meth = cls.getMethod(n)
        typecheckCall(meth, args, state)
        meth.ty
      }
      case _ => {
        Log.error("Method call on non-method " + id)
        "Object"
      }
    }
  }
}

case class ClassCall(self: Expr, id: Expr, args: List[Expr]) extends Expr
                                                             with Callable{
  def typecheck(state: Map[String, String]) : String = {
    (self, id) match {
      case (Var(c), Var(m)) => {
        val clsopt = Main.prog.find({_.Name() == c})
        if (clsopt == None) {
          Log.error("class call on non-existent " + c + ".")
        }
        val meth = clsopt.get.getMethod(m)
        typecheckCall(meth, args, state)
        meth.ty
      }
      case _ => {
        Log.error("Inappropriate class call. " + self + " and "
                    + id + " must be ids.")
        "Object"
      }
    }
  }
}

trait Let {
  def load(state: Map[String, String])
  def typecheck(state: Map[String, String]) : String
}

case class LetPln(name: String, ty: String) extends Let {
  def load(state: Map[String, String]) = {
    state(name) = ty
  }

  def typecheck(state: Map[String, String]) = ty
}

case class LetAsgn(name: String, ty:String, body: Expr) extends Let {
  def load(state: Map[String, String]) = {
    state(name) = ty
  }

  def typecheck(state: Map[String, String]) = {
    val bty = body.typecheck(state)
    if (bty != ty) {
      Log.error("Let " + name + " is not of declared type.")
    }
    ty
  }
}

abstract class Feature

case class Attribute(name: String, ty: String) extends Feature
case class Method(name: String, args:List[Attribute], ty:String, body: Expr) extends Feature


