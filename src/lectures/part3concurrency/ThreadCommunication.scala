package lectures.part3concurrency

import scala.collection.mutable.ArrayBuffer
import scala.util.Random

object ThreadCommunication extends App {

  /*
    The produced-consumer problem
   */
  object Container {
    var value = 0

    def isEmpty = value == 0

    def get: Int = {
      val toReturn = value
      value = 0
      toReturn
    }

    def set(v: Int): Unit = {
      value = v
    }
  }
  def dumbProdCons(): Unit = {
    val consumeThread = new Thread(() => {
      while (Container.isEmpty) {
        println("[consumer] waiting")
      }
      println("[consumer] consumed " + Container.get)
    })

    val produceThread = new Thread(() => {
      Thread.sleep(1000)
      println("[producer] produced value")
      Container.set(1023)
    })

    consumeThread.start()
    produceThread.start()
  }

  def smartProdCon(): Unit = {
    val producer = new Thread(() => {
      println("Working")
      Thread.sleep(1000)
      Container.synchronized {
        Container.set(1023)
        Container.notify()
      }
    })

    val consumer = new Thread(() => {
      Container.synchronized {
        Container.wait()
      }
      println("Consumed " + Container.get)
    })

    producer.start()
    consumer.start()
  }

  //smartProdCon()

  /*
    producer -> [ ? ? ? ] -> consumer
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

  def multiProdCons: Unit = {
    val producer = new Thread(() => {
      while (true) {
        MultiContainer.synchronized {
          if (MultiContainer.isFull) {
            MultiContainer.wait()
          }
          println("[consumer] setting")
          MultiContainer.set(Random.nextInt(10) + 1)
          MultiContainer.notify()
        }
      }
    })

    val consumer = new Thread(() => {
      while (true) {
        MultiContainer.synchronized {
          if (MultiContainer.isEmpty) {
            MultiContainer.wait()
          }
          println("[producer] getting " + MultiContainer.get)
          MultiContainer.notify()
        }
      }
    })

    producer.start()
    consumer.start()
  }
  //multiProdCons

  /*
    producer1 -> [ ? ? ? ] <- consumer1
    producer2 -----^   ^----- consumer2
    */

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

  def multiAgentProdCons: Unit = {
    (1 to 10).foreach(new Producer(_).start())
    (1 to 2).foreach(new Consumer(_).start())
  }
  multiAgentProdCons

  /*
    3 producers produce
    7 producers go into wait
    2 consumers consume 3 values. This leads to 3 producers being woken up.
    3 producers produce. the notify's go to other producers who go to sleep again. Now everyone is asleep.
   */
}
