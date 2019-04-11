package lectures.Excercises

abstract class MyQueue[A] {
  def remove: (A, MyQueue[A])
  def add(elem: A): MyQueue[A]
  def peek: A
}

class EmptyQueue[A] extends MyQueue[A] {
  override def remove: (A, MyQueue[A]) = throw new RuntimeException

  override def add(elem: A): MyQueue[A] = new NonEmptyMyQueue[A](elem, new EmptyQueue)

  override def peek: A = throw new RuntimeException
}

class NonEmptyMyQueue[A](head: A, tail: MyQueue[A]) extends MyQueue[A] {
  override def remove: (A, MyQueue[A]) = (head, tail)

  override def add(elem: A): MyQueue[A] = new NonEmptyMyQueue(head, tail.add(elem))

  override def peek: A = head
}

object MyQueue {
  def apply[A](values: A*): MyQueue[A] = {
    def buildQueue(toAdd: Seq[A], acc: MyQueue[A]): MyQueue[A] = {
      if (toAdd.isEmpty) acc
      else buildQueue(toAdd.tail, acc.add(toAdd.head))
    }
    buildQueue(values.toSeq, new EmptyQueue[A])
  }
}

object MyQueueTest extends App {
  val q = MyQueue(1,2,3,4)
  val smallerQ = q.remove._2
  println(smallerQ)
}