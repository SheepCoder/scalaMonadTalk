package monadic

trait Database {
  val lastNames = Map("1" -> "Richardson", "2" -> "Robinson", "4" -> "Smith").withDefaultValue(null)
  
  val firstNames = Map("1" -> "James", "2" -> "Simon", "3" -> "John").withDefaultValue(null)
}