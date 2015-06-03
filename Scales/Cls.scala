package scales
import scales.exprs._
import scala.collection.mutable.Map

class Cls (name: String, parent: String, feats: List[Feature]){

  override def toString() = ("class " + name + " inherits " + parent + " {" +
                              feats.mkString("; ") + "}")

  def getAttributes(prog: List[Cls]) : Map[String, String] = {
    var attrs = if (hasSuper()) {
      getSuper(prog).get.getAttributes(prog)
    } else {
      Map[String, String]()
    }
    for (feat <- feats) {
      feat match {
        case Attribute(n, t) => if (attrs.contains(n)) {
            Log.error("Duplicate field " + n + " in class " + name + ".")
          }
          attrs(n) = t
        case _ =>
      }
    }
    attrs
  }
  
  def getMethod(name : String) : Method = {
    val found = getMethods().find({_.name == name})
    if (found != None) {
      found.get
    } else if (hasSuper()){
      getSuper(Main.prog).get.getMethod(name)
    } else {
      println(Main.prog map {_.Name()})
      throw new IllegalArgumentException("Non-existent method " + name
                                          + " in object " + Name())
    }
  }

  def getMethods() : List[Method] = {
   var methods = List[Method]()
   for (feat <- feats) {
      feat match {
        case Method(n, a, t, b) => if (methods.find(x => x.name == n) != None) {
            Log.error("Duplicate method " + n + " in class " + name + ".")
          }
          methods +:= Method(n, a, t, b)
        case _ =>
      }
    }
    methods
  }

  def getSuper(prog: List[Cls]) : Option[Cls] = {
    prog.find({_.Name() == parent})
  }

  def hasSuper() = parent != ""

  def Feats() = feats
  def Name() = name
  def Parent() = parent

}
