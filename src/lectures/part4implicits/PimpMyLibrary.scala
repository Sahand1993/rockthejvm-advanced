package lectures.part4implicits

object PimpMyLibrary extends App {
  // 2.isPrime. Wouldn't it be great if we could do that?

  implicit class RichInt(val value: Int) extends AnyVal {
    def isEven: Boolean = value % 2 == 0
    def sqrt: Double = Math.sqrt(value)
    def times(f: () => Unit): Unit = (1 to value).foreach(_ => f())
    def *[A](list: List[A]): List[A] = {
      def appendNTimes(n: Int, acc: List[A], toAppend: List[A]): List[A] = {
        if (n <= 0) acc
        else appendNTimes(n - 1, acc ++ list, list)
      }
      appendNTimes(value, List(), list)
    }
  }

  implicit class RicherInt(richInt: RichInt) {
    def isOdd: Boolean = richInt.value % 2 != 0
  }
  println(new RichInt(42).sqrt)
  println(42 isEven)
  1 to 10 // this works the same way

  import scala.concurrent.duration._
  3.seconds

  // compiler does not do multiple implicit searches. Meaning it doesn't wrap more than once. RichInt isn't wrapped to RicherInt
  //42.isOdd

  /**
    * Enrich the String class
    * - add asInt method (parses the string and returns an int
    * - encrypt method (Caesar cipher)
    *   John -> Lqjp
    *
    *   Keep enriching the Int class
    *   - times(funciton)
    *     3.times(() => ...)
    *   - *
    *     3 * List(1,2) => List(1,2,1,2,1,2)
   */


  implicit class RichChar(val value: Char) extends AnyVal {
    def isLowerCase: Boolean = value.toInt >= 97 && value.toInt <= 122
    def isUpperCase: Boolean = value.toInt >= 65 && value.toInt <= 90
  }

  implicit class RichString(val value: String) extends AnyVal {
    def asInt: Int = Integer.valueOf(value)

    def encrypt(increment: Int): String = value.map(incrementChar(_, increment))

    def incrementChar(char: Char, increment: Int): Char = {
      if (char.isLowerCase) {
        incrementLowerCase(char, increment)
      }
      else if (char.isUpperCase) {
        incrementUpperCase(char, increment)
      }
      else throw new IllegalArgumentException("Char was neither upper nor lower case")
    }

    def incrementLowerCase(char: Char, increment: Int): Char = {
      incrementAnyCase(char, increment, 97, 122)
    }

    def incrementUpperCase(char: Char, increment: Int): Char = {
      incrementAnyCase(char, increment, 65, 90)
    }

    def incrementAnyCase(char: Char, increment: Int, caseLowerBound: Int, caseUpperBound: Int) = {
      val incremented = char + increment
      if (incremented > caseUpperBound)
        (caseLowerBound + (incremented - caseUpperBound) - 1).toChar
      else incremented.toChar
    }
  }
  println("Z" encrypt 2)
  println('Z'.toInt)
  println("10".asInt + 4)
  println(10.times(() => println("hi again")))

  println(4 * List(1,2,3))

  // "3" / 4 like in Javascript. Can we do this? Yes, we can.
  implicit def stringToInt(string: String): Int = Integer.valueOf(string)
  println("6" / 2)

  // equivalent to: implicit class RichAltInt(value: Int)
  class RichAltInt(value: Int)
  implicit def enrich(value: Int): RichAltInt = new RichAltInt(value)

  // danger zone
  implicit def intToBoolean(i: Int): Boolean = i == 1

  /*
    if (n)
      doSomething()
    else
      doSomethingElse()
   */

  val aConditionedValue = if (3) "OK" else "Something wrong"
  println(aConditionedValue)

  // TIPS: Only use implicit and type classes. Avoid implicit defs

}
