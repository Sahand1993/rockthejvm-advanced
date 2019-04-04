package lectures.Excercises

import scala.annotation.tailrec

trait MySet[A] extends (A => Boolean){
  /**
    * Excercise: Implement a functional set
    */
  override def apply(elem: A): Boolean = contains(elem)
  def contains(elem: A): Boolean
  def +(elem: A): MySet[A]
  def ++(anotherSet: MySet[A]): MySet[A]
  def isEmpty: Boolean

  def map[B](f: A => B): MySet[B]
  def flatMap[B](f: A => MySet[B]): MySet[B]
  def filter(predicate: A => Boolean): MySet[A]
  def forEach(f: A => Unit): Unit
}

class Empty[A]() extends MySet[A] {
  override def contains(elem: A): Boolean = false
  override def +(elem: A): MySet[A] = new Cons[A](elem, new Empty);
  override def ++(anotherSet: MySet[A]): MySet[A] = anotherSet
  override def isEmpty: Boolean = true;

  override def map[B](f: A => B): MySet[B] = new Empty[B]
  override def flatMap[B](f: A => MySet[B]) = new Empty[B]
  override def filter(predicate: A => Boolean): MySet[A] = this
  override def forEach(f: A => Unit): Unit = ()
}

class Cons[A](head: A, tail: MySet[A]) extends MySet[A] {
  override def contains(elem: A): Boolean = head.equals(elem) || tail.contains(elem)

  override def +(elem: A): MySet[A] = {
    if (!contains(elem))
      new Cons(elem, this)
    else
      this
  }

  override def ++(anotherSet: MySet[A]): MySet[A] = tail ++ anotherSet + head

  override def map[B](f: A => B): MySet[B] = tail.map(f) + f(head)

  override def flatMap[B](f: A => MySet[B]): MySet[B] = f(head) ++ tail.flatMap(f)

  override def filter(predicate:  A => Boolean): MySet[A] = {
    if (predicate(head)) {
      tail.filter(predicate) + head
    } else {
      tail.filter(predicate)
    }
  }

  override def forEach(f: A => Unit): Unit = {
    f(head)
    tail.forEach(f)
  }

  override def isEmpty: Boolean = false
}

object MySet {
  def apply[A](values: A*): MySet[A] = {
    @tailrec
    def buildSet(valSeq: Seq[A], acc: MySet[A]): MySet[A] =
      if (valSeq.isEmpty) acc
      else buildSet(valSeq.tail, acc + valSeq.head)
    buildSet(values.toSeq, new Empty)
  }
}

object MySetTest extends App {
  val s = MySet(1,2,3,4)
  s + 5 ++ MySet(-1, -2) + 3 flatMap (x => MySet(x, x * 10)) filter(_ % 2 == 0) forEach println
}