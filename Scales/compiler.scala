import exprs._
import Uncool._

object Main {

  private var errors = List[String]()
  private var prog = List[Cls]()
  private val builtins = List("Int", "Int[]", "String", "Object")

  //private var self :public static Class self;

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

  def logError(err: String) = {
    errors = errors :+ err
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

  def typecheckClass(clas: Cls) = {
    if (!builtins.contains(clas.Name())) {
      if (clas.hasSuper() && clas.getSuper(prog) == None) {
        logError(clas.Name() + " inherits from undefined " + clas.Parent())
      }
      val state = Attribute("self", clas.Name()) :: clas.getAttributes(prog) 
      clas.getMethods().foreach{a => typecheckMethod(a, state)}
      println("TODO")
    }
  }

  def typecheckMethod(meth: Method, state: List[Attribute]) = {
    /*Map<String,String> methodState=new HashMap<String,String>(typeMap);
    ArrayList<String> argNames=new ArrayList<String>();*/
    var argNames = List[String]()
    for(arg <- meth.args)
    {
      //UncoolAid 2.1.2 says that method params hide attrbiutes.
      if(argNames.contains(arg.name))
      {
        logError("Duplicate " + arg.name + " in " + meth.name + " args.")
      }
      argNames +:= arg.name
      //state.put(arg.id,arg.ty);
    }
    println(meth.name +":" + argNames)
    /*String ty= getBody().typecheck(methodState);
    if(!ty.equals(type))
    {
      logError(lineno,toString()+" returns "+ty+" declares "+type);
    }
    setClass(self.name);*/
  }

  def main(args: Array[String]) = {
    val ast : List[Cls] = Uncool.make_ast(args(0))
    if (!findMain(ast)) {
      logError("Main not found.")
    }
    prog = makeBuiltIns(ast)
    prog.foreach{typecheckClass}
    if (errors.length > 0) {
      errors.foreach{println}
    } else {
      //TODO Compile
    } 
  }
}
