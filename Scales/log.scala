package scales

object Log {
  
  var errors = List[String]()
  def error(err : String) = {
    errors :+= err
  }
}
