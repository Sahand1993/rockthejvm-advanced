package lectures.part3concurrency

import scala.concurrent.{Await, Future, Promise}
import scala.util.{Failure, Random, Success, Try}
import scala.concurrent.duration._

// important for futures
import scala.concurrent.ExecutionContext.Implicits.global

object FuturesAndPromises extends App {
  def calculateMeaningOfLife: Int = {
    Thread.sleep(2000)
    42
  }

  val aFuture = Future {
    calculateMeaningOfLife // Calculates the meaning of life on another thread
  } // (global) is passed by the compiler

  println(aFuture.value) // Option[Try[Int]]
  println("Waiting on the future...")
  aFuture.onComplete {
    case Success(meaningOfLife) => println(s"The meaning of life is $meaningOfLife")
    case Failure(e) => println(s"I have failed with $e")
  }

  Thread.sleep(3000)

  // mini social network

  case class Profile(id: String, name: String) {
    def poke(anotherProfile: Profile) = {
      println(s"${this.name} poking ${anotherProfile.name}")
    }
  }

  object SocialNetwork {
    // "database"
    val names = Map(
      "fb.id.1-zuck" -> "Mark",
      "fb.id.2-bill" -> "Bill",
      "fb.id.0-dummy" -> "Dummy"
    )
    val friends = Map(
      "fb.id.1-zuck" -> "fb.id.2-bill"
    )

    val random = new Random()

    // API
    def fetchProfile(id: String): Future[Profile] = Future {
      // Fetching from database
      Thread.sleep(random.nextInt(300))
      Profile(id, names(id))
    }

    def fetchBestFriend(profile: Profile): Future[Profile] = Future {
      Thread.sleep(random.nextInt(400))
      val bfId = friends(profile.id)
      Profile(bfId, names(bfId))
    }
  }

  // client: mark to poke bill
  val mark = SocialNetwork.fetchProfile("fb.id.1-zuck")

  mark.onComplete({
    case Success(markProfile) => {
      val bill = SocialNetwork.fetchBestFriend(markProfile)
      bill.onComplete({
        case Success(billProfile) => {
          markProfile.poke(billProfile)
        }
        case Failure(e) => e.printStackTrace()
      })
    }
    case Failure(e) => e.printStackTrace()
  })
  // Functional composition of futures
  // map, flatMap, filter
  val nameOnTheWall = mark.map(profile => profile.name)
  val marksBestFriend = mark.flatMap(profile => SocialNetwork.fetchBestFriend(profile))
  val zucksBestFriendRestricted = marksBestFriend.filter(profile => profile.name.startsWith("Z"))

  // for-comprehensions
  for {
    mark <- SocialNetwork.fetchProfile("fb.id.1-zuck")
    bill <- SocialNetwork.fetchBestFriend(mark)
  } mark.poke(bill)
  // equivalent to:
  SocialNetwork.fetchProfile("fb.id.1-zuck").foreach(mark => SocialNetwork.fetchBestFriend(mark).foreach(bill => mark.poke(bill)))

  // fallbacks
  val aProfileNoMatterWhat = SocialNetwork.fetchProfile("unknown id").recover {
    case e: Throwable => Profile("fb.id.0-dummy", "Forever alone")
  }

  val aFetchedProfileNoMatterWhat = SocialNetwork.fetchProfile("unknown id").recoverWith {
    case e: Throwable => SocialNetwork.fetchProfile("fb.id.0-dummy")
  }

  val fallbackResult = SocialNetwork.fetchProfile("unknown id").fallbackTo(SocialNetwork.fetchProfile("fb.id.0-dummy"))

  // online banking app
  case class User(name: String)
  case class Transaction(sender: String, receiver: String, amount: Double, status: String)

  object BankingApp {
    val name = "Rock the JVM banking"

    def fetchUser(name: String): Future[User] = Future {
      // simulate fetching from the DB
      Thread.sleep(500)
      User(name)
    }

    def createTransaction(user: User, merchantName: String, amount: Double): Future[Transaction] = Future {
      // simulate some processes
      Thread.sleep(1000)
      Transaction(user.name, merchantName, amount, "SUCCESS")
    }

    def purchase(username: String, item: String, merchantName: String, cost: Double): String = {
      val transactionStatus = for {
        user <- fetchUser(username)
        transaction <- createTransaction(user, merchantName, cost)
      } yield transaction.status
      // WAIT for the transaction to finish
      Await.result(transactionStatus, 2.seconds) // implicit conversions -> pimp my library
    }
  }

  println(BankingApp.purchase("Sahand", "Gibson Les Paul", "Guitar Center", 3000))

  // promises
  val promise = Promise[Int]() // "controller" over a future
  val future = promise.future

  // thread 1 - "consumer"
  future.onComplete {
    case Success(r) => println("[consumer] I've received " + r)
  }

  // thread 2 - "producer"
  val producer = new Thread(() => {
    println("[producer] crunching numbers...")
    Thread.sleep(1000)
    // "fulfilling" the promise
    promise.success(42)
    println("[producer] done")
  })

  producer.start()

  /**
    * Excercises
    * 1) Fulfill a future immediately with a value
    * 2) Run a function which returns a future after a future has finished running
    * 3) first(fa, fb) => new future with the value of the future that finished first
    * 4) last(fa, fb) => new future
    * 5) retryUntil[T](action: () => Future[T], condition: T => Boolean): Future[T]
   */

  // 1)
  val myPromise = Promise[Int]()
  val myFuture = myPromise.future
  myPromise.success(10)
  myFuture.onComplete {
    case Success(value) => println(s"Future completed with value $value")
  }

  // 2)
  def inSequence(f1: Future[Int], f2: Future[Int]): Future[Int] =
    f1.flatMap(_ => f2)


  val promise1 = Promise[Int]()
  promise1.future.onComplete({
    case Success(value) => println(s"promise 1 completed with value $value")
  })
  val promise2 = Promise[Int]()
  new Thread(() => println(inSequence(promise1.future, promise2.future))).start()
  Thread.sleep(100)
  promise1.success(11)
  Thread.sleep(100)
  promise2.success(99)

  // 3)
  def first[A](f1: Future[A], f2: Future[A]): Future[A] = {
    val promise = Promise[A]()

//    def tryComplete(future: Future[A]) = {
//      future.onComplete {
//        case Success(value) => {
//          promise.synchronized {if (!promise.isCompleted) promise.success(value)}
//        }
//        case Failure(e) => {
//          promise.synchronized {if (!promise.isCompleted) promise.failure(e)}
//        }
//      }
//    }

    def tryComplete(promise: Promise[A], result: Try[A]): Unit = {
      result match {
        case Success(value) => try {
          promise.success(value)
        } catch {
          case _ =>
        }
        case Failure(e) => try {
          promise.failure(e)
        } catch {
          case _ =>
        }
      }
    }

    //tryComplete(f1)
    //tryComplete(f2)
    //f1.onComplete(t => tryComplete(promise, t))
    //f2.onComplete(t => tryComplete(promise, t))
    // best way (using built in function on promise):
    f1.onComplete(promise.tryComplete)
    f2.onComplete(promise.tryComplete)
    promise.future
  }
  val promise3 = Promise[Int]()
  val promise4 = Promise[Int]()
  val future3 = promise3.future
  val future4 = promise4.future

  val resultFuture = first(future3, future4)
  promise3.success(11)
  Thread.sleep(100)
  promise4.success(10)
  resultFuture.onComplete({
    case Success(value) => println("The result of future is: " + value)
  })

  /*
    4)
   */

  def last[A](f1: Future[A], f2: Future[A]): Future[A] = {
    var promise1 = Promise[A]()
    var promise2 = Promise[A]()

    def tryCompleteAlternative(f: Future[A]): Unit = {
      f.onComplete(t => t match {
        case Success(value) => {
          try {
            promise1.success(value)
          } catch {
            case _ => promise2.success(value)
          }
        }
        case Failure(e) => {
          try {
            promise1.failure(e)
          } catch {
            case _ => promise2.failure(e)
          }
        }
      })
    }

    tryCompleteAlternative(f1)
    tryCompleteAlternative(f2)

    promise2.future
  }

  val promise5 = Promise[Int]()
  val promise6 = Promise[Int]()
  val future5 = promise5.future
  val future6 = promise6.future

  val lastFuture = last(future5, future6)

  promise5.success(10)
  Thread.sleep(100)
  promise6.success(21)
  for {
    value <- lastFuture
  } println("The last future had the value " + value)


  /*
    5)
   */

  def retryUntil[T](action: () => Future[T], condition: T => Boolean): Future[T] = {
    action().flatMap(value => if (condition(value)) Promise[T]().success(value).future else retryUntil(action, condition))
  }
  var x = 0
  val validFuture = retryUntil(() => {
    x += 1
    Promise[Int]().success(x).future
  }, (x: Int) => x > 10)

  validFuture.onComplete({
    case Success(x) => println("The valid x is " + x)
  })

  Thread.sleep(2000)
}
