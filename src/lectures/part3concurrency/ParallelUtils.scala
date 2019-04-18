package lectures.part3concurrency

import scala.collection.parallel.immutable.ParVector

object ParallelUtils extends App {
  // 1 parallel collections

  val parList = List(1,2,3).par

  val parVector = ParVector[Int](1,2,3)

  /*
    Seq
    Vector
    Array
    Map - Hash, Trie
    Set - Hash, Trie
   */

  def measure[T](operation: => T): Long = {
    val time = System.currentTimeMillis()
    operation
    System.currentTimeMillis() - time
  }

  val list = (1 to 10000000).toList
  val serialTime = measure {
    list.map(_ + 1)
  }
  println("serial time: " + serialTime)

  val parallelTime = measure {
    list.par.map(_ + 1)
  }
  println("parallel time: " + parallelTime)
}
