package lectures.part1advancedscala

import scala.util.Try

object DarkSugar extends App {

  // #1: methods with single parameter
  def singleArgMethod(arg: Int): String = s"$arg little ducks"

  val description = singleArgMethod{
    // write some code
    42
  }
  // for example:
  val aTryInstance = Try { // throwing Try's apply method with the code block as argument
    throw new RuntimeException
  }
  // another example: providing the lambda function with code block
  List(1,2,3).map { x =>
    x + 1
  }

  // #2: Single abstract method
  trait Action {
    def act(x: Int): Int
  }

  val anInstance: Action = (x: Int) => {
    x + 1
  }

  // example
  val aThread = new Thread(new Runnable {
    override def run(): Unit = println("Hello, Scala")
  })

  val aSweeterThread = new Thread(() => println("Hello, Scala"))
  // example
  abstract class AnAbstractType {
    def implementedMethod: Int = 23
    def abstractMethod(a: Int): Unit
  }

  val anImplementation: AnAbstractType = (a: Int) => println(a)

  // #3: the :: and #:: methods are special

  val prependedList = 2 :: List(3, 4)
  val equivalent = List(3, 4).::(2)

  //Every method with a name that ends with a : is right-associative
  class MyStream[T] {
    def -->:(value: T): MyStream[T] = this
  }
  val stream = new MyStream[Int]
  1 -->: 2 -->: 3 -->: stream

  // btw: #:: is the prepend operator on streams

  // #4: multi-word method naming
  class TeenGirl(name: String) {
    def `and then said`(gossip: String) = println(name + " said " + gossip)
  }
  val lily = new TeenGirl("Lily")
  lily `and then said` "Scala rocks"

  // #5: infix types

  class Composite[A, B]

  val composite: Int Composite String = new Composite // same as Composite[Int, String]

  class -->[A, B]
  val instance: Int --> String = new -->

  // #6: update() is special, much like apply()
  val anArray = Array(1,2,3)
  anArray(2) = 7
  anArray.update(2, 7) // equivalent

  // #7 setters for mutable containers
  class Mutable {
    private var internalMember: Int = 0 // private for OO encapsulation
    def member = internalMember // Getter
    def member_=(value: Int): Unit =
      internalMember = value // Setter
  }

  val aMutableContainer = new Mutable
  aMutableContainer.member = 42 // equivalent to aMutableContainer.member_=(42)
}
