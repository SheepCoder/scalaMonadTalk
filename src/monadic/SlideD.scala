package monadic

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
}