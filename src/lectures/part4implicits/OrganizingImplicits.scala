package lectures.part4implicits

import scala.math.Ordering

object OrganizingImplicits extends App{
  implicit val reverseOrdering: Ordering[Int] = Ordering.fromLessThan(_ > _)
  println(List(1,4,5,3,2).sorted)
  // scala.Predef contains an implicit ordering for a collection of Ints.

  /*
    Implicits:
      - val/var
      - object
      - accessor methods = defs with no parentheses
   */

  // Excercise
  case class Person(name: String, age: Int)

  val persons = List(
    Person("Steve", 30),
    Person("Amy", 22),
    Person("John", 66)
  )

  // 1. Define an implicit ordering that enables you to sort persons.
//  object Person {
//    implicit val alphabeticalOrdering: Ordering[Person] = Ordering.fromLessThan(_.name < _.name)
//  }
 // implicit val ageOrdering: Ordering[Person] = Ordering.fromLessThan(_.age < _.age)
  /*
    Implicit scope
    - normal scope = LOCAL SCOPE
    - imported scope
    - companion objects of all types involved in the method signature
      for sorted below:
      - List
      - Ordering
      - All the types involved = A or any supertype
   */

  //def sorted[B >: A](implicit ord: Ordering[B]): List[B]
  object AlphabeticNameOrdering {
    implicit val alphabeticOrdering: Ordering[Person] = Ordering.fromLessThan(_.name < _.name)
  }

  object AgeOrdering {
    implicit val ageOrdering: Ordering[Person] = Ordering.fromLessThan(_.age < _.age)
  }

  //import AgeOrdering._
  import AlphabeticNameOrdering._
  println(persons.sorted)

  /*
    Excercise

    Add 3 orderings:
    - TotalPrice = most used (70%)
    - unit count = used by 20%
    - unit price = 10%

   */
  case class Purchase(nUnits: Int, unitPrice: Double)

  object Purchase {
    implicit val totalPriceOrdering: Ordering[Purchase] = Ordering.fromLessThan((a, b) => a.nUnits * a.unitPrice < b.nUnits * b.unitPrice)
  }
  object UnitCountOrdering {
    implicit val unitCountOrdering: Ordering[Purchase] = Ordering.fromLessThan(_.nUnits < _.nUnits)
  }
  object UnitPriceOrdering {
    implicit val unitPriceOrdering: Ordering[Purchase] = Ordering.fromLessThan(_.unitPrice < _.unitPrice)
  }

  val purchases = List(
    new Purchase(10, 200),
    new Purchase(12, 150),
    new Purchase(100, 100)
  )
  //import UnitPriceOrdering._
  //import UnitCountOrdering._
  println(purchases.sorted)

}
