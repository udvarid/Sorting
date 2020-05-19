package donat.com.myArray

import scala.annotation.tailrec
import scala.util.Random

case class MyArray(size: Int, unique: Boolean = false, range: Int = 1000) {

  val array: Array[Int] = initArray(Array[Int]())

  @tailrec
  private def initArray(arr: Array[Int]): Array[Int] = {
    if (arr.size == size) arr
    else {
      val ranNumber = Random.nextInt(range)
      if (unique && arr.contains(ranNumber)) initArray(arr)
      else initArray(arr :+ ranNumber)
    }
  }

  private def schuffle: Array[Int] = ???

  private def selectionSort: Array[Int] = ???

  private def insertionSort: Array[Int] = ???

  private def shellSort: Array[Int] = ???

  private def mergeSort: Array[Int] = ???

  private def quickSort: Array[Int] = ???


}
