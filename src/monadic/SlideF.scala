package monadic

/**
 * Reusing our code to provide a trace framework
 */
object SlideF {
  
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
  
  def _return[S,A](value : A): State[S, A] = new State[S, A](s => (s, value))
 
  def _runState[S, A](state: State[S, A], initialState: S) = state.stateFunction(initialState)
  
  // A trace framework
  val add = (x: Int, y: Int) => new State[List[String], Int](s => (s :+ ((x + " + " + y + " = " + (x + y)).toString), x + y))
  val sub = (x: Int, y: Int) => new State[List[String], Int](s => (s :+ ((x + " - " + y + " = " + (x - y)).toString), x - y))
  
  def main(args: Array[String]): Unit = {
    val f = (i: Int) => _return(i) >>= {j => add(j , 5)} >>= {k => sub(k, 6)}
    println(_runState(f(3), List()))
  }
}