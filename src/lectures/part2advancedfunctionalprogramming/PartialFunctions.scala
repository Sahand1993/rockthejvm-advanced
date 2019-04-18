package lectures.part2advancedfunctionalprogramming

object PartialFunctions extends App {

  val aFunction = (x: Int) => x + 1 // Function1[Int, Int] === Int => Int
  // Important property: Any int can be inserted into the function and yield a result === the function is defined on the whole domain.

  // Functions that only accepted certain parts of the input domain:
  val aFussyFunction = (x: Int) =>
    if (x == 1) 42
    else if (x == 2) 56
    else if (x == 5) 999
    else throw new FunctionNotApplicableException
  // Clunky.
  class FunctionNotApplicableException extends RuntimeException

  // Using pattern matching instead
  val aNicerFussyFunction = (x: Int) => x match {
    case 1 => 42
    case 2 => 56
    case 5 => 999
  }
  // aNicerFussyFunction is a function {1,2,5} => Int
  // aNicerFussyFunction is called a partial function because it's defined on a part of the Int domain

  // short hand notation for partial functions:
  val aPartialFunction: PartialFunction[Int, Int] = {
    case 1 => 42
    case 2 => 56
    case 5 => 999
  } // partial function value

  println(aPartialFunction(2))
  //println(aPartialFunction(1123))

  // PF utilities
  println(aPartialFunction.isDefinedAt(2))

  // lift
  val lifted = aPartialFunction.lift // turns partial function into Function[Int, Option[Int]]
  println(lifted(1)) // Some(42)
  println(lifted(123)) // None

  val pfChain = aPartialFunction.orElse[Int, Int] {
    case 45 => 67
  }
  println(pfChain(1)) // Some(42) from aPartialFunction
  println(pfChain(45)) // Some(67) from the orElse argument-function

  // PF extend normal functions
  val aTotalFunction: Int => Int = {
    case 1 => 99
  }

  // HOFs accept partial functions as well
  val aMappedList = List(1,2,3).map {
    case 1 => 42
    case 2 => 78
    case 3 => 1000
  }
  println(aMappedList)

  /*
  Note: PF can only have 1 parameter type
   */

  /**
    * Excercises
    *
    * 1 - construct a partial function instance yourself (anonymous class)
    * 2 - implement a small chatbot as a partial function
   */

  // 1.
  val myPartial: PartialFunction[String, Int] = {
    case "1" => 1
    case "2" => 2
  }
  val myPartialAnonymous: PartialFunction[String, Int] = new PartialFunction[String, Int] {
    override def isDefinedAt(x: String): Boolean = {
      try {
        apply(x)
        true
      } catch {
        case _: MatchError => false
      }
    }

    override def apply(str: String): Int = str match {
      case "1" => 1
      case "2" => 2
    }
  }
  println(myPartialAnonymous("2"))

  // 2.
  val chatbot: PartialFunction[String, String] = {
    case "Gruezi mittenand!" => "Gruezi!"
  }
  io.Source.stdin.getLines().map(chatbot).foreach(println)
}
