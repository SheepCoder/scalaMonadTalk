package monadic

/**
 * Reuse of code: An example using maths and the Maybe monad.
 */
object SlideC {
  trait Maybe[T] {
    def >>=(f: (T => Maybe[T])): Maybe[T]
    
    def lift: T
    
    def isEmpty: Boolean
  }
    
  case class Nothing[T] extends Maybe[T] {
    override def >>=(f: (T => Maybe[T])):  Maybe[T] = new Nothing[T]
    
    override def lift: T = throw new NoSuchElementException("Nothing cannot contain a value")
    
    override def isEmpty: Boolean = true
  }
    
  case class Some[T](val value: T) extends Maybe[T] {
    override def >>=(f: (T => Maybe[T])): Maybe[T] = f(value)
    
    override def lift: T = value
    
    override def isEmpty: Boolean = false
  }
  
  def _return[T](v: T) = Some(v)
  
  def add(x: Int, y: Int): Int = x + y
  
  def sub(x: Int, y: Int): Int = x - y
  
  def div(top: Int, bottom: Int): Int = top / bottom
  
  def doMath(start: Int):Int = {
    add(div(10, sub(add(start, 3), 8)), 4)
  }
  
  def doMathExpanded(start: Int): Int = {
    val add3: Int => Int = i => add(i, 3)
    val sub8: Int => Int = i => sub(i, 8)
    val div10: Int => Int = i => div(10, i)
    val add4: Int => Int = i => add(i, 4)
    
    add4(div10(sub8(add3(start))))
  }
  
  def doMathMonadic(start: Int): Int = {
    val add3: Int => Maybe[Int] = i => _return(add(i, 3))
    val sub8: Int => Maybe[Int] = i => _return(sub(i, 8))
    val div10: Int => Maybe[Int] = i => {
      if (i == 0) {
        Nothing()
      } else {
        _return(div(10, i))
      }
    }
    
    val add4: Int => Maybe[Int] = i => _return(add(i, 4))
    
    def doMath: Int => Maybe[Int] = i => _return(i) >>= add3 >>= sub8 >>= div10 >>= add4
    
    doMath(start).lift
  }
  
  def main(args: Array[String]): Unit = {
    println("Standard")
    println("3 => " + doMath(3))
    println("10 => " + doMath(10))
    //println("5 => " + doMath(5))
    
//    println("Expanded")
//    println("3 => " + doMathExpanded(3))
//    println("10 => " + doMathExpanded(10))
    //println("5 => " + doMathExpanded(5))

    //    println("Monadic")
//    println("3 => " + doMathMonadic(3))
//    println("10 => " + doMathMonadic(10))
    //println("5 => " + doMathMonadic(5))
  }
}