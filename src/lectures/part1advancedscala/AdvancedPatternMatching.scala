package lectures.part1advancedscala

object AdvancedPatternMatching extends App {

  val numbers = List(1)
  val description = numbers match {
    case head :: Nil => println(s"The only element is $head.")
    case _ =>
  }

  /*
  Structures available for pattern matching are:
  - Constants
  - Wildcards
  - Case classes
  - Tuples
  - Some special magic (above)
    */

  class Person(val name: String, val age: Int)
  // let's say for some reason you can not make this a case class, but you still want to be able to pattern match instances of this class.
  // What to do?
  object PersonPattern { // does not need to have same name. But it SHOULD, for best practice
    def unapply(person: Person): Option[(String, Int)] =
      if (person.age < 21) null
      else Some((person.name, person.age))

    // we can overload the unapply method
    def unapply(age: Int): Option[String] =
      Some(if (age < 21) "minor" else "adult")
  }
  val bob = new Person("Bob", 25)
  val greeting = bob match {
    case PersonPattern(name, age) => s"Hi, my name is $name and I am $age years old."
  }

  val legalStatus = bob.age match {
    case PersonPattern(status) => s"My legal status is $status"
  }

  println(legalStatus)

  /*
  Excercise
   */
  val n: Int = 1
  val mathProperty = n match {
    case x if x < 10 => "Single digit"
    case x if x % 2 == 0 => "Even"
    case _ => "something else"
  } // write this more elegantly using custom Pattern matching

  // my solution
  object NumberMatcher {
    def unapply(n: Int): Option[String] =
      Some({
        if (n < 10) "Single digit"
        else if (n % 2 == 0) "Even"
        else "something else"
      })
  }

  val cleanerMathProperty = n match {
    case NumberMatcher(message) => message
  }

  // other possible solution
  object even {
    def unapply(n: Int): Boolean = n % 2 == 0
  }

  object singleDigit {
    def unapply(n: Int): Boolean = n < 10 && n > -10
  }

  val anotherCleanMathProperty = n match {
    case singleDigit() => "Single digit"
    case even() => "Even"
    case _ => "Something else"
  }
  println(anotherCleanMathProperty)


  // infix patterns
  case class Or[A, B](a: A, b: B) // called Either in Scala
  val either = Or(2, "two")
  val humanDescription = either match {
    case number Or string => s"$number is written as $string"
  }
  println(humanDescription)

  // decomposing sequences
  val vararg = numbers match {
    case List(1, _*) => "Starting with one"
  }

  abstract class MyList[+A] {
    def head: A = ???
    def tail: MyList[A] = ???
  }

  object Empty extends MyList[Nothing]
  case class Cons[+A](override val head: A, override val tail: MyList[A]) extends MyList[A]

  object MyList {
    def unapplySeq[A](list: MyList[A]): Option[Seq[A]] =
      if (list == Empty) Some(Seq.empty)
      else unapplySeq(list.tail).map(list.head +: _)
  }

  val myList = Cons(1, Cons(2, Cons(3, Empty)))
  val decomposed = myList match {
    case MyList(1,2,_*) => "Starting with 1, 2"
    case _ => "Something else"
  }
  println(decomposed)

  println(MyList.unapplySeq(myList)) // Why does this not print a Seq??

  abstract class Wrapper[T] {
    def isEmpty: Boolean
    def get: T
  }

  object PersonWrapper {
    def unapply(person: Person): Wrapper[String] = new Wrapper[String] {
      def isEmpty = false
      def get = person.name
    }
  }

  println(bob match {
    case PersonWrapper(n) => n
  })
}
