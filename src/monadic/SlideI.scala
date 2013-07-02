package monadic

/**
 * But Monads are just a type of function, so they are just a type of arrow
 */
object SlideI {
  
  // Maybe monad
  trait Maybe[T] {
    def >>=(f: (T => Maybe[T])): Maybe[T]
  }
    
  case class Nothing[T] extends Maybe[T] {
    override def >>=(f: (T => Maybe[T])):  Maybe[T] = new Nothing[T]
  }
    
  case class Some[T](val value: T) extends Maybe[T] {
    override def >>=(f: (T => Maybe[T])): Maybe[T] = f(value)
  }
  
   def _returnMaybe[T](v: T) = Some(v)
   
  // State Monad
  class State[S, A](val stateFunction: S => (S, A)) {
    def >>=(bindFunction: A => State[S, A]): State[S, A] = {
      new State(
        s => {
          val pair1 = _runState(this, s)
          val newState = bindFunction(pair1._2)
          _runState(newState, pair1._1)
        })
    }
  }
  
  def _returnState[S,A](value : A): State[S, A] = new State[S, A](s => (s, value))
 
  def _runState[S, A](state: State[S, A], initialState: S) = state.stateFunction(initialState)
  
  // Can we extract an interface?
  type ??? = Any
  
  trait Monad[A] {
    def >>=(x: ???): ???
  }
  
  // So, what is a Kleisli Arrow?
  class Arrow[A, B](arrow: A => B) {
    def apply(a: A): B = arrow(a)
    
    def >>>[C](otherArrow: Arrow[B, C]) = arr[A, C](a => otherArrow(arrow(a)))
  }
  
  def arr[X, Y](arrow: X => Y) = new Arrow(arrow)
  
  trait KleisliArrow[A, ???] {
    def >>>[C](otherArrow: KleisliArrow[C, ???])
  }
}