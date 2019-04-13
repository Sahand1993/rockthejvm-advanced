package excercises

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
  def -(elem: A): MySet[A]
  def --(other: MySet[A]): MySet[A]
  def &(other: MySet[A]): MySet[A]
  def unary_! : MySet[A]

  def map[B](f: A => B): MySet[B]
  def flatMap[B](f: A => MySet[B]): MySet[B]
  def filter(predicate: A => Boolean): MySet[A]
  def foreach(f: A => Unit): Unit
}

class EmptySet[A]() extends MySet[A] {
  override def contains(elem: A): Boolean = false
  override def +(elem: A): MySet[A] = new NonEmptySet[A](elem, new EmptySet);
  override def ++(anotherSet: MySet[A]): MySet[A] = anotherSet
  override def isEmpty: Boolean = true;
  override def -(elem: A): MySet[A] = this
  override def --(other: MySet[A]): MySet[A] = this
  override def &(other: MySet[A]): MySet[A] = this
  override def unary_! : MySet[A] = new PropertyBasedSet(_ => true)

  override def map[B](f: A => B): MySet[B] = new EmptySet[B]
  override def flatMap[B](f: A => MySet[B]) = new EmptySet[B]
  override def filter(predicate: A => Boolean): MySet[A] = this
  override def foreach(f: A => Unit): Unit = ()
}

class PropertyBasedSet[A](property: A => Boolean) extends MySet[A] {
  override def contains(elem: A): Boolean = property(elem)

  override def +(elem: A): MySet[A] =
    new PropertyBasedSet[A](potentialMember => property(potentialMember) || potentialMember == elem)

  override def ++(anotherSet: MySet[A]): MySet[A] = new PropertyBasedSet[A](
    potentialMember => property(potentialMember) || anotherSet(potentialMember)
  )

  override def isEmpty: Boolean = ???

  override def -(elem: A): MySet[A] = filter(_ != elem)

  override def --(other: MySet[A]): MySet[A] = filter(!other)

  override def &(other: MySet[A]): MySet[A] = filter(other)

  override def unary_! : MySet[A] = new PropertyBasedSet[A](potentialMember => !property(potentialMember))

  def politelyFail = throw new IllegalArgumentException("Really deep rabbit hole")

  override def map[B](f: A => B): MySet[B] = politelyFail

  override def flatMap[B](f: A => MySet[B]): MySet[B] = politelyFail

  override def foreach(f: A => Unit): Unit = politelyFail

  override def filter(predicate: A => Boolean): MySet[A] = new PropertyBasedSet[A](
    potentialMember => predicate(potentialMember) && property(potentialMember)
  )

}

class NonEmptySet[A](head: A, tail: MySet[A]) extends MySet[A] {
  override def contains(elem: A): Boolean = head.equals(elem) || tail.contains(elem)

  override def +(elem: A): MySet[A] = {
    if (!contains(elem))
      new NonEmptySet(elem, this)
    else
      this
  }

  override def ++(anotherSet: MySet[A]): MySet[A] = tail ++ anotherSet + head

  override def isEmpty: Boolean = false

  override def -(elem: A): MySet[A] = {
    if(head.equals(elem)) tail
    else {
      tail - elem + head
    }
  }

  override def --(other: MySet[A]): MySet[A] = filter(!other)

  override def &(other: MySet[A]): MySet[A] = filter(other) // intersection and filtering are the same concepts

  override def unary_! : MySet[A] = new PropertyBasedSet[A](
    potentialMember => !contains(potentialMember)
  )

  override def map[B](f: A => B): MySet[B] = tail.map(f) + f(head)

  override def flatMap[B](f: A => MySet[B]): MySet[B] = f(head) ++ tail.flatMap(f)

  override def filter(predicate:  A => Boolean): MySet[A] = {
    if (predicate(head)) {
      tail.filter(predicate) + head
    } else {
      tail.filter(predicate)
    }
  }

  override def foreach(f: A => Unit): Unit = {
    f(head)
    tail.foreach(f)
  }

}

object MySet {
  def apply[A](values: A*): MySet[A] = {
    @tailrec
    def buildSet(valSeq: Seq[A], acc: MySet[A]): MySet[A] =
      if (valSeq.isEmpty) acc
      else buildSet(valSeq.tail, acc + valSeq.head)
    buildSet(values.toSeq, new EmptySet)
  }
}

object MySetTest extends App {
  val s1 = MySet(1,2,3,4)
  val s2 = MySet(3,4,5,6)
  println(s1 -- s2 foreach println)
  println(s1 map(_ => 1) foreach println)

  val negative = !s1 // s1.unary_! = all the naturals except 1, 2, 3 and 4.

  val negativeEven = negative.filter(_ % 2 == 0)
  println((negativeEven + 5)(5))
  println(negativeEven(7))
}