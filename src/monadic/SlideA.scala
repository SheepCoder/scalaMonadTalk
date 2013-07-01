package monadic

object SlideA {

  val lastNames = Map("1" -> "Richardson", "2" -> "Robinson").withDefaultValue(null)
  
  val firstNames = Map("1" -> "James", "2" -> "Simon").withDefaultValue(null)
  
  def lastName(id: String): String = lastNames(id)
  
  def firstName(id: String): String = firstNames(id)
  
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
  
  def main(args: Array[String]): Unit = {
    println("1 -> " + fullName("1"))
    println("2 -> " + fullName("2"))
    println("3 -> " + fullName("3"))
  }
}


trait Maybe[T] {
  
}

object Nothing extends Maybe[Any] {
  
}

class Some[T](val value: T) extends Maybe[T] {
  
}
