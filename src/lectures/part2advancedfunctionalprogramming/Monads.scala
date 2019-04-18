package lectures.part2advancedfunctionalprogramming

object Monads extends App {

  // Our own try-monad
  trait Attempt[+A] {
    def flatMap[B](f: A => Attempt[B]): Attempt[B]
  }
  object Attempt {
    def apply[A](a: => A): Attempt[A] = try {
      Success(a)
    } catch {
      case e: Throwable => Fail(e)
    }
  }
  case class Success[A](value: A) extends Attempt[A] {
    def flatMap[B](f: A => Attempt[B]): Attempt[B] = {
      try {
        f(value)
      } catch {
        case e: Throwable => Fail(e)
      }
    }
  }
  case class Fail(e: Throwable) extends Attempt[Nothing] {
    def flatMap[B](f: Nothing => Attempt[B]): Attempt[B] = this
  }
  val attempt = Attempt {
    throw new RuntimeException("My own monad")
  }
  println(attempt)


  object Lazy {
    def apply[A](value: => A): Lazy[A] = new Lazy(value)
  }
  class Lazy[A](v: => A){
    lazy val internal = v
    def flatMap[B](f: (=> A) => Lazy[B]): Lazy[B] = f(internal)
    def use = v
  }
  val lazyInstance = Lazy {
    println("Message in a Lazy")
    42
  }
  val flatMappedLazy1 = lazyInstance.flatMap(x => Lazy {
    10 * x
  })
  val flatMappedLazy2 = lazyInstance.flatMap(x => Lazy {
    10 * x
  })
  flatMappedLazy1.use
  flatMappedLazy2.use

  /*
    left-identity:

    lazy.flatMap(f) = f(x) must hold, and it does

    right-identity:

    lazy.flatMap(unit) = lazy
    lazy.flatMap(x => Lazy(x)) = lazy

    associativity:

    lazy.flatMap(f).flatMap(g)  = lazy.flatMap(x => f(x).flatMap(g))
    f(v).flatMap(g) = f(v).flatMap(g)
   */

  def map[A, B](list: List[A], f: A => B): List[B] = list.flatMap(x => List(f(x)))

  def flatten[A](list: List[List[A]]): List[A] = list.flatMap(x => x)

  val list = List(1,2,3)
  println(map(list, (x: Int) => 3 * x))
  val listsInList = List(List(1,2,3), List(4,5,6), List(7,8,9))
  println(flatten(listsInList))
  /*
  flatMap(x => f(x)) = f(a) ++ f(b) ++ f(c)
  map(g) = List(g(a), g(b), g(c))
   */

}
