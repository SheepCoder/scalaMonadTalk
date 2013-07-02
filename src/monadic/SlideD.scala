package monadic

/**
 * Another common problem: Passing state through methods
 */
object SlideD {

  type MyStack = List[Int]
  
  def push(stack: MyStack, x: Int) = x :: stack
  
  def pop(stack: MyStack) = (stack.tail, stack.head)
  
  def peek(stack: MyStack) = (stack, stack.head)
  
  def main(args: Array[String]): Unit = {
    val initialStack: MyStack = List(2, 3)
    
    val stack1 = pop(initialStack)._1
    val stack2 = push(stack1, 3)
    val stack3 = push(stack2, 4)
    
    val finalStack = {
        if (peek(stack3)._2 == 4) {
          val stack4 = pop(stack3)._1
          push(stack4, 9)
        } else {
          push(stack3, 8)
        }
    }
    
    // produces List(9, 3, 3)
    println(finalStack)
  }
  
  /**
   * Adding a class doesn't help if we want to be immutable
   */
  def evenWithClassesWeHaveSimilarProblem() {
    class SomeOtherStack(val contents: List[Int]) {
      def push(value: Int) = new SomeOtherStack(value :: contents)
      
      def peek: Int = contents.head
      
      def pop: SomeOtherStack = new SomeOtherStack(contents.tail)
    }
    
    val initialStack = new SomeOtherStack(List(2, 3))
    
    val stack1 = initialStack.pop
    val stack2 = stack1.push(3)
    val stack3 = stack2.push(4)
    
    // etc...
  }
}