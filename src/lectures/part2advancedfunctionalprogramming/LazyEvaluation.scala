package lectures.part2advancedfunctionalprogramming

object LazyEvaluation extends App{

  // lazy delays the evaluation of values
  lazy val x: Int = {
    println("hello")
    42
  }
  println(x)
  println(x)

  // examples of implications:
  def sideEffectCondition: Boolean = {
    println("boo")
    true
  }
  def simpleCondition: Boolean = false

  lazy val lazyCondition = sideEffectCondition
  println(if (simpleCondition && lazyCondition) "yes" else "no")

  def byNameMethod(n: => Int): Int = {
    lazy val t = n
    t + t + t + 1
  }
  def retrieveMagicValue = {
    // side effect or a long computation
    println("Waiting")
    Thread.sleep(1000)
    42
  }
  println(byNameMethod(retrieveMagicValue))
  // use lazy vals instead

  // filtering with lazy vals
  def lessThan30(i: Int): Boolean = {
    println(s"$i is less than 30?")
    i < 30
  }

  def greaterThan20(i: Int): Boolean = {
    println(s"$i is greater than 20?")
    i > 20
  }

  val numbers = List(1, 25, 40, 5, 23)
  val lt30 = numbers.filter(lessThan30)
  val gt20 = lt30.filter(greaterThan20)
  println(gt20)

  val lt30lazy = numbers.withFilter(lessThan30)
  val gt20lazy = lt30lazy.withFilter(greaterThan20)
  gt20lazy.foreach(println)

  // for-comprehensions use withFilter with guards
  for {
    a <- List(1,2,3) if a % 2 == 0 // if-guards use lazy vals
  } yield a + 1
  List(1,2,3).withFilter(_ % 2 == 0).map(_ + 1)

  /*
    Excercise: implement a lazily evaluated, singly linked Stream of elements.

    naturals = MyStream.from(1)(x => x + 1) = stream of natural numbers (infinite)
    naturals.take(100).foreach(println) // lazily evaluated stream of the first 100 naturals
    naturals.foreach(println) // will crash because naturals is infinite
    naturals.map(_ * 2) // stream of all even numbers
   */
}
