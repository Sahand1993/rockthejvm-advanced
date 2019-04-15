package lectures.part3concurrency

import scala.concurrent.{Await, Future, Promise}
import scala.util.{Failure, Random, Success}
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
   */

  // 1)
  val myPromise = Promise[Int]()
  val myFuture = myPromise.future
  myPromise.success(10)
  myFuture.onComplete {
    case Success(value) => println(s"Future completed with value $value")
  }

  // 2)
  def inSequence(f1: Future[Int], f2: Future[Int]): Int = {
    Await.result(f1, 2.seconds)
    Await.result(f2, 2.seconds)
  }

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
  def first(f1: Future[Int], f2: Future[Int]): Future[Int] = {
    val result = Promise[Int]()
    f1.onComplete {
      case Success(value) => result.success(value)
    }

    f2.onComplete {
      case Success(value) => result.success(value)
    }
    result.future
  }
  // TODO: Test this. What happens when success is called in the onComplete of the future that finishes last?


  Thread.sleep(2000)


}
