import exprs._

class Cls (name: String, parent: String, feats: List[Feature]){

  override def toString() = ("class " + name + " inherits " + parent + " {" +
                              feats.mkString("; ") + "}")

  def getAttributes(prog: List[Cls]) : List[Attribute] = {
    var attrs = if (hasSuper()) {
      getSuper(prog).get.getAttributes(prog)
    } else {
      List[Attribute]()
    }
    for (feat <- feats) {
      feat match {
        case Attribute(n, t) => if (attrs.find(x => x.name == n) != None) {
            Main.logError("Duplicate field " + n + " in class " + name + ".")
          }
          attrs +:= Attribute(n,t)
        case _ =>
      }
    }
    attrs
  }

  def getMethods() : List[Method] = {
   var methods = List[Method]()
   for (feat <- feats) {
      feat match {
        case Method(n, a, t, b) => if (methods.find(x => x.name == n) != None) {
            Main.logError("Duplicate method " + n + " in class " + name + ".")
          }
          methods +:= Method(n, a, t, b)
        case _ =>
      }
    }
    methods
  }

  def getSuper(prog: List[Cls]) : Option[Cls] = {
    prog.find(a => a.Name() == parent)
  }

  def hasSuper() = parent == ""

  def Feats() = feats
  def Name() = name
  def Parent() = parent

}
