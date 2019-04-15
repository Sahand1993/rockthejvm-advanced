package lectures.part3concurrency

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer
import scala.util.Random

  object ThreadCommunication extends App {

    /*
      the producer-consumer problem
      producer -> [ ? ] -> consumer
     */
    class SimpleContainer {
      private var value: Int = 0

      def isEmpty: Boolean = value == 0
      def set(newValue: Int) = value = newValue
      def get = {
        val result = value
        value = 0
        result
      }
    }

    def naiveProdCons(): Unit = {
      val container = new SimpleContainer

      val consumer = new Thread(() => {
        println("[consumer] waiting...")
        while(container.isEmpty) {
          println("[consumer] actively waiting...")
        }

        println("[consumer] I have consumed " +  container.get)
      })

      val producer = new Thread(() => {
        println("[producer] computing...")
        Thread.sleep(500)
        val value = 42
        println("[producer] I have produced, after long work, the value " + value)
        container.set(value)
      })

      consumer.start()
      producer.start()
    }

    // naiveProdCons()

    // wait and notify
    def smartProdCons(): Unit = {
      val container = new SimpleContainer

      val consumer = new Thread(() => {
        println("[consumer] waiting...")
        container.synchronized {
          container.wait()
        }

        // container must have some value
        println("[consumer] I have consumed " + container.get)
      })

      val producer = new Thread(() => {
        println("[producer] Hard at work...")
        Thread.sleep(2000)
        val value = 42

        container.synchronized {
          println("[producer] I'm producing " + value)
          container.set(value)
          container.notify()
        }
      })

      consumer.start()
      producer.start()
    }

    // smartProdCons()

    /*
      producer -> [ ? ? ? ] -> consumer
     */

    def prodConsLargeBuffer(): Unit = {
      val buffer: mutable.Queue[Int] = new mutable.Queue[Int]
      val capacity = 3

      val consumer = new Thread(() => {
        val random = new Random()

        while(true) {
          buffer.synchronized {
            if (buffer.isEmpty) {
              println("[consumer] buffer empty, waiting...")
              buffer.wait()
            }

            // there must be at least ONE value in the buffer
            val x = buffer.dequeue()
            println("[consumer] consumed " + x)

            // hey producer, there's empty space available, are you lazy?!
            buffer.notify()
          }

          Thread.sleep(random.nextInt(250))
        }
      })

      val producer = new Thread(() => {
        val random = new Random()
        var i = 0

        while(true) {
          buffer.synchronized {
            if (buffer.size == capacity) {
              println("[producer] buffer is full, waiting...")
              buffer.wait()
            }

            // there must be at least ONE EMPTY SPACE in the buffer
            println("[producer] producing " + i)
            buffer.enqueue(i)

            // hey consumer, new food for you!
            buffer.notify()

            i += 1
          }

          Thread.sleep(random.nextInt(500))
        }
      })

      consumer.start()
      producer.start()
    }

    // prodConsLargeBuffer()

    /*
      Prod-cons, level 3
           producer1 ->  [ ? ? ? ] -> consumer1
           producer2 -----^     ^---- consumer2
     */

    object MultiContainer {
      var values: ArrayBuffer[Int] = ArrayBuffer(0,0,0)

      def isEmpty: Boolean = !values.exists(_ != 0)
      def isFull: Boolean = !values.contains(0)

      def get: Int = {
        val nonZeroIndex = getRandomNonZeroIndex
        val returnValue = values(nonZeroIndex)
        values(nonZeroIndex) = 0
        returnValue
      }

      def set(value: Int): Unit = {
        values(getRandomZeroIndex) = value
      }

      def getRandomZeroIndex: Int = {
        val zeroIndexes = values
          .zipWithIndex
          .filter(valAndIndex => valAndIndex._1 == 0)
          .map(valAndIndex => valAndIndex._2)
        val randomZeroIndex = zeroIndexes(Random.nextInt(zeroIndexes.size))
        randomZeroIndex
      }

      def getRandomNonZeroIndex: Int = {
        val nonZeroIndexes = values
          .zipWithIndex
          .filter(valAndIndex => valAndIndex._1 != 0)
          .map(valAndIndex => valAndIndex._2)
        val randomNonZeroIndex = nonZeroIndexes(Random.nextInt(nonZeroIndexes.size))
        randomNonZeroIndex
      }
    }


    class Producer(id: Int) extends Thread {
      override def run(): Unit = {
        while (true) {
          MultiContainer.synchronized {
            while (MultiContainer.isFull) {
              MultiContainer.wait()
            }
            val produce = Random.nextInt(10)
            MultiContainer.set(produce)
            MultiContainer.notifyAll()
            println(s"producer$id produced $produce")
          }
        }
      }
    }

    class Consumer(id: Int) extends Thread {
      override def run(): Unit = {
        while (true) {
          MultiContainer.synchronized {
            while (MultiContainer.isEmpty) {
              MultiContainer.wait()
            }
            MultiContainer.notifyAll()
            println(s"consumer$id consumed " + MultiContainer.get)
          }
        }
      }
    }

    def multiProdCons(nConsumers: Int, nProducers: Int): Unit = {
      (1 to nConsumers).foreach(new Consumer(_).start())
      (1 to nProducers).foreach(new Producer(_).start())
    }
  //multiProdCons(2, 10)
    /*
      Excercises:
      1) think of an example where notifyAll acts in a different way than notify?
          - notifyAll will solve the deadlock issue. I don't know how
      2) create a deadlock
          - See above with notify() instead of notifyAll()
      3) create a livelock
          - What does it mean to not be blocked but can't continue?
     */

    // notifyAll
    def testNotifyAll(): Unit = {
      val bell = new Object

      (1 to 10).foreach(i => new Thread(() => {
        bell.synchronized {
          println(s"[thread $i] waiting")
          bell.wait()
          println(s"[thread $i] woken up!")
        }
      }).start())

      new Thread(() => {
        Thread.sleep(2000)
        println("[announcer] rock n roll")
        bell.synchronized {
          bell.notify()
        }
      }).start()
    }

    //testNotifyAll()

    // deadlock
    case class Friend(name: String) {
      def bow(other: Friend) = {
        this.synchronized {
          println(s"$this: I am bowing to my friend $other")
          other.rise(this)
          println(s"$this: my friend $other has risen")
        }
      }

      def rise(other: Friend) = {
        this.synchronized {
          println(s"$this: I am rising to my friend $other")
        }
      }

      var side = "right"
      def giveWay(other: Friend): Unit = {
        while(this.side == other.side) {
          //println(s"$this: Please, $other, feel free to pass.")
          switchSide()
          Thread.sleep(1000)
        }
      }
      def switchSide(): Unit = {
        side = if (side == "right") "left" else "right"
        println(s"$this switched side to $side")
      }
    }


    val sam = Friend("Sam")
    val pierre = Friend("Pierre")
    //new Thread(() => sam.bow(pierre)).start()
    //new Thread(() => pierre.bow(sam)).start()

    // livelock
    new Thread(() => {
      sam.giveWay(pierre)
    }).start()

    new Thread(() => {
      pierre.giveWay(sam)
    }).start()
}
