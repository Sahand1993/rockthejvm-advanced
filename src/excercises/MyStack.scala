package excercises

import scala.annotation.tailrec

abstract class MyStack[A] {
  def pop(): (A, MyStack[A])
  def push(elem: A): MyStack[A]
}

class EmptyStack[A] extends MyStack[A] {
  override def pop(): (A, MyStack[A]) = throw new RuntimeException

  override def push(elem: A) = new ConsStack(elem, new EmptyStack[A])
}

class ConsStack[A](val head: A, val tail: MyStack[A]) extends MyStack[A] {
  override def pop(): (A, MyStack[A]) = (head, tail)

  override def push(elem: A): MyStack[A] = new ConsStack[A](elem, new ConsStack[A](head, tail))
}

object MyStack {
  def apply[A](values: A*): MyStack[A] = {
    @tailrec
    def buildStack(acc: MyStack[A], toPush: Seq[A]): MyStack[A] = {
      if (toPush.isEmpty) acc
      else buildStack(acc push toPush.head, toPush.tail)
    }
    buildStack(new EmptyStack[A], values.toSeq)
  }
}

object MyStackTest extends App {
  val stack = MyStack("1", "2", "3")
  println(stack.pop())
}
