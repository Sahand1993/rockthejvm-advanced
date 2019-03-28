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
  val n: Int = 10
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
  println(cleanerMathProperty)
}
