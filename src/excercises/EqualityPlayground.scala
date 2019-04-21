package excercises

import lectures.part4implicits.TypeClasses.User

object EqualityPlayground extends App {
  /**
    * Equality
    */
  trait Equal[T] {
    def apply(a: T, b: T): Boolean
  }

  object UserAgeEquality extends Equal[User] {
    override def apply(a: User, b: User): Boolean = a.age == b.age
  }

  object UserNameEquality extends Equal[User] {
    override def apply(a: User, b: User): Boolean = a.name == b.name
  }

  implicit object UserFullEquality extends Equal[User] {
    override def apply(a: User, b: User): Boolean = a.name == b.name && a.age == b.age && a.email == b.email
  }

  println(UserAgeEquality(User("Tom", 3, "tommy@hotmail.com"), User("Adam", 2, "adamsandler@sandler.com")))
  println(UserNameEquality(User("Tom", 2, "asdf@asdf.com"), User("Tom", 3, "234sd@hotmail.com")))

  /**
    * Excercise: Implement the type class pattern for the Equality type class.
    */
  object Equal {
    def apply[T](a: T, b: T)(implicit equal: Equal[T]): Boolean = equal(a, b)
  }

  implicit object IntEqual extends Equal[Int] {
    override def apply(a: Int, b: Int): Boolean = a == b
  }

  implicit object StringEqual extends Equal[String] {
    override def apply(a: String, b: String): Boolean = a == b
  }

//  implicit object AnyEqual extends Equal[AnyVal] {
//    override def apply(a: AnyVal, b: AnyVal): Boolean = a == b
//  }

//  println(Equal(1,1))
//  println(Equal(User("Tom", 20, "tom@tom.se"), User("Tom", 20, "tom@tom.se"))) // uses the implicit UserFullEquality above
  // AD-HOC polymorphism

  implicit class EqualEnrichment[T](value: T) {
    def ===(anotherValue: T)(implicit equal: Equal[T]): Boolean = equal(value, anotherValue)
    def !==(anotherValue: T)(implicit equal: Equal[T]): Boolean = !equal(value, anotherValue)
  }

  println(1 === 1)

  val john = User("John", 32, "john@rockthejvm.com")
  println(john === john)
  new EqualEnrichment[Int](1).===(1)
}
