package excercises

import scala.annotation.tailrec

abstract class MyStream2nd[+A] {
  def isEmpty: Boolean
  def head: A
  def tail: MyStream2nd[A]

  def #::[B >: A](element: B): MyStream2nd[B] // prepend operator
  def ++[B >: A](anotherStream: => MyStream2nd[B]): MyStream2nd[B]

  def foreach(f: A => Unit): Unit
  def map[B](f: A => B): MyStream2nd[B]
  def flatMap[B](f: A => MyStream2nd[B]): MyStream2nd[B]
  def filter(predicate: A => Boolean): MyStream2nd[A]

  def take(n: Int): MyStream2nd[A] // takes first n elements out of the stream

  def takeAsList(n: Int): List[A] = {
    take(n).toList()
  }

  @tailrec
  final def toList[B >: A](acc: List[B] = Nil): List[B] = {
      if (isEmpty) acc.reverse
      else tail.toList(head :: acc)
    }
}

object MyStream {
  def from[A](start: A)(generator: A => A): MyStream2nd[A] = {
    new Cons(start, MyStream.from(generator(start))(generator))
  }

  def fibonacci: MyStream2nd[Int] = {
    // setup
    new Cons(0, fibonacciRest(0, 1))
  }

  def fibonacciRest(last: Int, curr: Int): MyStream2nd[Int] = {
    new Cons(curr, fibonacciRest(curr, last + curr))
  }
/*
    primes =
    new Cons(1, restOfPrimes(2, Set(1)) =
    new Cons(1, restOfPrimes(3, Set(1)) =
    new Cons(1,
 */
  def primes: MyStream2nd[Int] = {
    new Cons(1, restOfPrimes(2, Set()))
  }

  def restOfPrimes(start: Int, primes: Set[Int]): MyStream2nd[Int] = {
    if (hasFactorIn(start, primes)) restOfPrimes(start + 1, primes)
    else new Cons(start, restOfPrimes(start + 1, primes + start))
  }

  def hasFactorIn(number: Int, primes: Set[Int]): Boolean = {
    primes.exists(n => number % n == 0)
  }
}

object EmptyStream extends MyStream2nd[Nothing] {
  override def isEmpty: Boolean = true

  override def head: Nothing = ???

  override def tail: MyStream2nd[Nothing] = ???

  override def #::[B >: Nothing](element: B): MyStream2nd[B] = new Cons(element, this)

  override def ++[B >: Nothing](anotherStream: => MyStream2nd[B]): MyStream2nd[B] = anotherStream

  override def foreach(f: Nothing => Unit): Unit = ()

  override def map[B](f: Nothing => B): MyStream2nd[B] = this

  override def flatMap[B](f: Nothing => MyStream2nd[B]): MyStream2nd[B] = this

  override def filter(predicate: Nothing => Boolean): MyStream2nd[Nothing] = this

  override def take(n: Int): MyStream2nd[Nothing] = this
}

class Cons[+A](hd: A, tl: => MyStream2nd[A]) extends MyStream2nd[A] {
  override def isEmpty: Boolean = false

  override val head: A = hd

  override lazy val tail: MyStream2nd[A] = tl

  override def #::[B >: A](element: B): MyStream2nd[B] = new Cons(element, this)

  override def ++[B >: A](anotherStream: => MyStream2nd[B]): MyStream2nd[B] = {
    new Cons(head, tail ++ anotherStream)
  }

  override def foreach(f: A => Unit): Unit = {
    f(head)
    tail.foreach(f)
  }

  override def map[B](f: A => B): MyStream2nd[B] = {
    new Cons(f(head), tail.map(f))
  }

  override def flatMap[B](f: A => MyStream2nd[B]): MyStream2nd[B] = f(head) ++ tail.flatMap(f)

  override def filter(predicate: A => Boolean): MyStream2nd[A] = {
    if (predicate(head)) new Cons(head, tail.filter(predicate))
    else tail.filter(predicate)
  }

  override def take(n: Int): MyStream2nd[A] = {
    if (n <= 0) EmptyStream
    else if (n == 1) new Cons(head, EmptyStream)
    else new Cons(head, tail.take(n - 1))
  }
}

object Test extends App {
  val stream = MyStream.from(1)((n: Int) => n + 1)
  val naturals = MyStream.from(1)(_ + 1)
  val fromZero = 0 #:: naturals
  println(fromZero.head)

  println(fromZero.map(_ * 2).takeAsList(10))
  println(fromZero.flatMap(x => new Cons(x, new Cons(x + 1, EmptyStream))).take(10).toList())
 // fromZero.filter(_ < 0)
  val justZero = fromZero.filter(_ < 1)

  val fibonacciStream = MyStream.fibonacci // 1 1 2 3
  println(fibonacciStream.take(10).toList())

  val primesStream = MyStream.primes
  println(primesStream.take(10).toList())
}