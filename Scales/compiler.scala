package scales
import exprs._
import scala.collection.mutable.Map

object Main {

  var prog = List[Cls]()
  val builtins = List("Int", "Int[]", "String", "Object")

  def findMain(prog: List[Cls]) : Boolean = {
    var found = false
    val Main = prog.find(a => a.Name() == "Main")
    if (Main != None) {
      for (feat <- Main.get.Feats()) {
        feat match {
          case Method("main", _, _, _) => found = true 
          case _ =>
        }
      }
    }
    found
  }

  def makeBuiltIns(clses: List[Cls]) : List[Cls] = {
    val tyInt = new Cls("Int", "", List())
    val tyArr = new Cls("Int[]", "", List())
    val tyStr = new Cls("String", "", List())
    val abort = Method("abort", List(),"Object", Constant("Int", "0"))
    val in_int = Method("in_int", List(),"Int", Constant("Int", "0"))
    val in_string = Method("in_string", List(),"String", Constant("Int", "0"))
    val out_int = Method("out_int", List(Attribute("", "Int")),"Int",
      Constant("Int", "0"))
    val out_string = Method("out_string", List(Attribute("", "String")),
      "String", Constant("Int", "0"))
    val tyObj = new Cls("Object", "", List(abort, in_int, in_string, out_int,
      out_string))
    List[Cls](tyInt, tyArr, tyStr, tyObj) ++ clses
  }

  def Prog() : List[Cls] = prog

  def typecheckClass(clas: Cls) = {
    if (!builtins.contains(clas.Name())) {
      if (clas.hasSuper() && clas.getSuper(prog) == None) {
        Log.error(clas.Name() + " inherits from undefined " + clas.Parent())
      }
      var typemap = clas.getAttributes(prog)
      typemap("self") = clas.Name()
      
      clas.getMethods().foreach{a => typecheckMethod(a, typemap)}
    }
  }

  def typecheckMethod(meth: Method, state: Map[String, String]) = {
    var methodState = state.clone()
    var argNames = List[String]()
    for(arg <- meth.args)
    {
      //UncoolAid 2.1.2 says that method params hide attrbiutes.
      if(argNames.contains(arg.name))
      {
        Log.error("Duplicate " + arg.name + " in " + meth.name + " args.")
      }
      argNames +:= arg.name
      methodState(arg.name) = arg.ty
    }
//    println(meth.name +":" + argNames)
    val ty = meth.body.typecheck(methodState);
    if(ty != meth.ty)
    {
      Log.error(meth.name + " returns " + ty + " declares " + meth.ty)
    }
    //TODO setClass(self.name);*/
  }

  def main(args: Array[String]) = {
    val ast : List[Cls] = Uncool.make_ast(args(0))
    if (!findMain(ast)) {
      Log.error("Main not found.")
    }
    prog = makeBuiltIns(ast)
    prog.foreach{typecheckClass}
    if (Log.errors.length > 0) {
      println("The following type errors were found:")
      Log.errors.foreach{println}
    } else {
      //TODO Compile
    } 
  }
}
