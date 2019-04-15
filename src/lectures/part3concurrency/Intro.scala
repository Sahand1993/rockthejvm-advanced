package lectures.part3concurrency

import java.util.concurrent.Executors

object Intro extends App {
  /*
    interface Runnable {
      public void run
     }
   */
  // JVM threads
  val runnable = new Runnable {
    override def run(): Unit = println("Running in parallel")
  }
  val aThread = new Thread(runnable)

  aThread.start() // gives signal to jvm to start os thread
  // create a JVM thread, runs on top of an OS thread
  runnable.run() // this doesn't do anything in parallel!

  aThread.join() // blocks until the thread has finished running.

  val threadHello = new Thread(() => (1 to 5).foreach(_ => println("hello")))
  val threadGoodbye = new Thread(() => (1 to 5).foreach(_ => println("goodbye")))
  threadHello.start()
  threadGoodbye.start()

  // executors
  val pool = Executors.newFixedThreadPool(10)
  pool.execute(() => println("something in the thread pool"))
  pool.execute(() => {
    Thread.sleep(1000)
    println("done after 1 second")
  })
  pool.execute(() => {
    Thread.sleep(1000)
    println("almost done")
    Thread.sleep(1000)
    println("done after 2 seconds")
  })
  pool.shutdown()
//  pool.execute(() => println("should not appear")) // throws exception in the calling thread
  //pool.shutdownNow() // interrupts threads
  println(pool.isShutdown)


  def runInParallel = {
    var x = 0

    val thread1 = new Thread(() => {
      x = 1
    })

    val thread2 = new Thread(() => {
      x = 2
    })

    thread1.start()
    thread2.start()
    println(x)
  }

  for (_ <- 1 to 1) runInParallel

  class BankAccount(@volatile var amount: Int) {
    override def toString: String = "" + amount
  }

  def buy(account: BankAccount, thing: String, price: Int) = {
    account.amount -= price
    //println(s"I've bought $thing")
    //println(s"My account is now $account")
  }

  for (_ <- 1 to 1) {
    val account = new BankAccount(50000)
    val thread1 = new Thread(() => buy(account, "shoes", 3000))
    val thread2 = new Thread(() => buy(account, "iPhone XII", 4000))

    thread1.start()
    thread2.start()
    Thread.sleep(10)
    if(account.amount != 43000) {
      println("aha!")
      println(account.amount)
    }
  }

  // how to solve race condition problem
  // option 1: use synchronized(). This is the most powerful option.
  def buySafe(account: BankAccount, thing: String, price: Int): Unit = {
    account.synchronized {
      // no two threads can evaluate this at the same time
      account.amount -= price
      println(s"I've bought $thing")
      println(s"My account is now $account")
    }
  }

  // option 2: use annotation @volatile


  /**
    * Excercises:
    *
    * 1) Construct 50 "Inception" threads (threads that construct other threads)
    *     thread1 -> thread2 -> thread3 -> ...
    *     println("Hello from thread1")
    *
    *     print these thread greetings in reversed order
    */

  def createThreads(n: Int): Thread = {
    if (n > 1) new Thread(() => {
      println("Hello from thread #" + n)
      createThreads(n - 1).start()
    })
    else new Thread(() => println("Hello from thread #"+n))
  }

  createThreads(50).start()

  /**
    * 2)
    */
  var x = 0
  val threads = (1 to 100).map(_ => new Thread(() => x += 1))
  threads.foreach(_.start())
  /*
    Questions:
    1. What is the biggest value possible for x? 100
    2. What is the smallest value possible for x? 1
   */

  /**
    * 3) sleep fallacy
    */
  var message = ""
  val awesomeThread = new Thread(() => {
    Thread.sleep(1000)
    message = "Scala is awesome"
  })

  message = "Scala sucks"
  awesomeThread.start()
  Thread.sleep(2000)
  println(message)

  /*
  What is the value of message? -> "Scala is awesome" because we're waiting 2 seconds.
  Is it guaranteed? Why? | Why not? -> pretty much guaranteed because 2 seconds is a long time
   */
  // How to be sure that awesomeThread finishes before printing:
  awesomeThread.start()
  Thread.sleep(2000)
  awesomeThread.join()
  println(message)
}
