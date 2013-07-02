package monadic

/**
 * Logging trace with Arrows
 */
object SlideH {

  // An Arrow
  class LoggingArrow[A, B](val name: String, arrow: A => B) {
    def apply(a: A): B = {
      // a simple log message
      println(name + "(" + a + ")")
      // do the action
      arrow(a)
    }
    
    def >>>[C](otherArrow: LoggingArrow[B, C]) = {
      val name = otherArrow.name + "(" + this.name + "(_))"
      new LoggingArrow[A, C](name, a => otherArrow(arrow(a)))
    }
  }
  
  def arr[X, Y](name: String, arrow: X => Y) = new LoggingArrow[X, Y](name, arrow)
  
  // Previous problem becomes
  val currentAge = arr[Int, Int]("currentAge", year => year - 1977)
  
  val convertToString = arr[Int, String]("convertToString", x => "" + x)
  
  val prettyPrint = arr[String, String] ("prettyPrint", s => s + "yrs")
  
  val printIt = arr[String, Int]("printIt", s => {println(s); s.length})
  
  def main(args: Array[String]):Unit = {
    // build our action
    val action = currentAge >>> convertToString >>> prettyPrint >>> printIt
    // run our action
    action(2013)
    
    // What about if we also log the function result?
  }
}