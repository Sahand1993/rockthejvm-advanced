package lectures.part2advancedfunctionalprogramming

object CurriesPartiallyAppliedFunctions extends App {

  // curried function = function that receives multiple parameter lists
  val superAdder: Int => Int => Int =
    x => y => x + y

  val add3 = superAdder(3) // Int => Int = y => 3 + y
  println(add3(5)) // 8
  println(superAdder(3)(5)) // 8

  // METHOD
  def curriedAdder(x: Int)(y: Int): Int = x + y

  val add4: Int => Int = curriedAdder(4) // lifting = ETA-EXPANSION

  // functions != methods (JVM limitation)
  def inc(x: Int) = x + 1
  List(1,2,3).map(inc) // compiler does ETA-expansion for us. It turns the inc method into a function, and then it uses that function with map.

  // Partial function applications
  val add5 = curriedAdder(5) _ // ETA-expansion. No need to provide type for add5.

  // Excercise
  val simpleAddFunction = (x: Int, y: Int) => x + y
  def simpleAddMethod(x: Int, y: Int) = x + y
  def curriedMethod(x: Int)(y: Int) = x + y

  // add7: Int => Int = y => 7 + y

  val add7_1 = (x: Int) => simpleAddFunction(x, 7)
  val add7_2 = (x: Int) => simpleAddMethod(x, 7)
  val add7_3 = (x: Int) => curriedMethod(x)(7)
  val add7_4: Int => Int = curriedMethod(7)
  val add7_5 = curriedMethod(7) _
  val add7_6 = simpleAddFunction.curried(7)
  val add7_7 = simpleAddMethod(7, _: Int)
  val add7_8 = simpleAddFunction(7, _: Int)


  println(add7_1(3))
  println(add7_2(3))
  println(add7_3(3))
  println(add7_4(3))
  println(add7_5(3))
  println(add7_6(3))
  println(add7_7(3))
  println(add7_8(3))

  // underscores are powerful
  def concatenator(a: String, b: String, c: String) = a + b + c
  val insertName = concatenator("Hello, I'm ", _: String, ", how are you?")
  println(insertName("Daniel"))

  val fillInTheBlanks = concatenator("Hello, ", _: String, _: String) // (x, y) => concatenator("Hello, ", x, y)
  println(fillInTheBlanks("I'm ", "Mahmoud."))

  // Excercises
  // 1. Process a list of numbers and return their string representations with different formats
  // Use the %4.2f, %8.6f and %14.12f with a curried formatter function.
  val formatter: String => Double => String = (format: String) => (number: Double) => format.format(number)
  val numbers = List(1231.3545, 234.2, 22.122778)
  val toFormat = formatter("%7.2f")
  numbers.map(toFormat).foreach(println)
  numbers.map(formatter("%14.8f"))
  /*
    2. Difference between
        - Functions vs methods
        - parameters: by-name vs 0-lambda
   */
  def byName(n: => Int) = n + 1
  def byFunction(f: () => Int) = f() + 1

  def method: Int = 42
  def parenthesisMethod(): Int = 42

  /*
  Calling byName and byFunction
    - Int
    - method
    - parenmethod
    - lambda
    - PAF
   */

  // Int
  byName(1)
  //println(byFunction(1))
  byFunction(() => 1)

  // method, parenthesisMethod
  byName(method)
  byName(parenthesisMethod)
  //println(byFunction(method)) // not ok! Compiler does not perform ETA-expansion here. Instead, it just calculates the value of method
  byFunction(parenthesisMethod)

  // lambda
  byName(1)
  byFunction(() => 1)

  // PAF
  val myFunc = (x: Int) => (y: Int) => x + y
  println(byName(myFunc(2)(2)))
  println(byFunction(() => myFunc(2)(2)))

}
