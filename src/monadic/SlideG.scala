package monadic

/**
 * Generalising Monads into Arrows
 */
object SlideG {

  def currentAge(year: Int):Int = year - 1977
  
  def convertToString(x: Int):String = "" + x
  
  def prettyPrint(s: String):String = s + "yrs"
  
  def printIt(s: String):Int = {println(s); s.length } 
  
  def main(args: Array[String]):Unit = {
    printIt(prettyPrint(convertToString(currentAge(2013))))
  }
  
  // can expand functions
  class Arrow[A, B](f: A => B) {
    def apply(a: A): B = f(a)
    
    // def >>>[C](otherArrow: Arrow[B, C]) = ?
  }
}