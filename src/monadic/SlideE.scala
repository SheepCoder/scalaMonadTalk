package monadic

object SlideE {
  

  // State Monad
  class State[S, A](val stateFunction: S => (S, A)) {
    def >>=(bindFunction: A => State[S, A]): State[S, A] = {
      new State[S, A](
        s => {
          val pair1 = _runState(this, s)
          val newState = bindFunction(pair1._2)
          _runState(newState, pair1._1)
        })
    }
  }
  
  def _return[S,A](value : A): State[S, A] = new State[S, A](s => (s, value))
  
  def _runState[S, A](state: State[S, A], initialState: S) = state.stateFunction(initialState)
  
  // Back into Stacks
  // -----------------
  type MyStack = List[Int]
  
  private def _pop(s: MyStack): (MyStack, Int) = (s.tail, s.head)
  
  def pop: Int => State[MyStack, Int] = a => new State[MyStack,Int](s => _pop(s))
  
  private def _push(s: MyStack, value: Int): (MyStack, Int) = (value :: s, -1)
  
  def push(value: Int): Int => State[MyStack, Int] = a => new State[MyStack, Int](s => _push(s, value))
 
  private def _peek(s: MyStack): (MyStack, Int) = (s, s.head)
  
  def peek: Int => State[MyStack, Int] = a => new State[MyStack,Int](s => _peek(s))
  
//def main(args: Array[String]): Unit = {
//    val initialStack: MyStack = List(2, 3)
//    
//    val stack1 = pop(initialStack)._1
//    val stack2 = push(stack1, 3)
//    val stack3 = push(stack2, 4)
//    
//    val finalStack = {
//        if (peek(stack3)._2 == 4) {
//          val stack4 = pop(stack3)._1
//          push(stack4, 9)
//        } else {
//          push(stack3, 8)
//        }
//    }
//    
//    // produces List(9, 3, 3)
//    println(finalStack)
//  }

 def main(args: Array[String]): Unit = {
    val initial = _return[MyStack, Int](-1)
    
    val actions = initial >>= pop >>= push(3) >>= push(4) >>= peek >>= (i => {
      if (i == 4) {
        _return(i) >>= pop >>= push(9)
      } else {
        push(8)(i)
      }
    })
    
    println(_runState(actions, List(2, 3)))
  }
}