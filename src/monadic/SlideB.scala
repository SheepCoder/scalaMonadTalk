package monadic

/**
 * Working towards a solution.  Allowing the Maybe monad to be combined.
 */
object SlideB extends Database {
  trait Maybe[T] {
    def combine(f: (T => Maybe[T])): Maybe[T]
    
    // def lift: T
  }
    
  case class Nothing[T] extends Maybe[T] {
    override def combine(f: (T => Maybe[T])):  Maybe[T] = new Nothing[T]
  }
    
  case class Some[T](val value: T) extends Maybe[T] {
    override def combine(f: (T => Maybe[T])): Maybe[T] = f(value)
  }
  
  def _return[T](v: T) = Some(v)
  
  def lastName(id: String): String = lastNames(id)
  
  def firstName(id: String): String = firstNames(id)
  
  def dbLookup(lookup : () => String): Maybe[String] = {
    val fromDb = lookup()
    
    if (fromDb == null) {
      Nothing()
    } else {
      _return(fromDb)
    }
  }
  
  def fullName(id: String): String = {
    val last = lastName(id)
    
    if (last != null) {
      val first = firstName(id)
      
      if (first != null) {
        first + " " + last
      } else {
        null
      }
    } else {
      null
    }
  }
  
  def fullNameMonadic(personId: String): Maybe[String] = 
      _return(personId)
          .combine(id => dbLookup(() => firstName(id))
              .combine(firstname => 
                    dbLookup(() => lastName(id))
                    .combine(lastName => _return(firstname + " " + lastName))))
          
  def main(args: Array[String]): Unit = {
    println("1 -> " + fullName("1"))
    println("2 -> " + fullName("2"))
    println("3 -> " + fullName("3"))
    println("4 -> " + fullName("4"))
    
    println("1 -> " + fullNameMonadic("1"))
    println("2 -> " + fullNameMonadic("2"))
    println("3 -> " + fullNameMonadic("3"))
    println("4 -> " + fullNameMonadic("4"))
  }
          
}