package donat.com.myArray

import scala.annotation.tailrec
import scala.util.Random

case class MyArray(size: Int, unique: Boolean, range: Int) {

  val array: Array[Int] = initArray(Array[Int]())

  @tailrec
  private def initArray(arr: Array[Int]): Array[Int] = {
    if (arr.length == size) arr
    else {
      val ranNumber = Random.nextInt(range)
      if (unique && arr.contains(ranNumber)) initArray(arr)
      else initArray(arr :+ ranNumber)
    }
  }

  def isItSorted(arrayToCheck: Array[Int]): Boolean = {
    if (arrayToCheck.tail.isEmpty) true
    else arrayToCheck.head <= arrayToCheck.tail.head && isItSorted(arrayToCheck.tail)
  }

  def schuffle: Array[Int] = {
    @tailrec
    def createSchuffled(schuffled: Array[Int], oldOne: Array[Int]): Array[Int] = {
      if (oldOne.length == 0) schuffled
      else {
        val exchangeIndex: Int = Random.nextInt(schuffled.length)
        val exchangeValue: Int = schuffled(exchangeIndex)
        schuffled(exchangeIndex) = oldOne.head
        createSchuffled(schuffled :+ exchangeValue, oldOne.tail)
      }
    }

    createSchuffled(Array[Int](array.head), array.tail)
  }

  def selectionSort: Array[Int] = {

    @tailrec
    def createSorted(sortedArray: Array[Int], oldArray: Array[Int]): Array[Int] = {
      if (oldArray.isEmpty) sortedArray
      else {
        val minValue: Int = oldArray.min
        val indexOfMin: Int = oldArray.indexOf(minValue)
        createSorted(sortedArray :+ minValue, oldArray.take(indexOfMin) ++ oldArray.takeRight(oldArray.length - indexOfMin - 1))
      }
    }

    createSorted(Array[Int](), array)
  }

  def insertionSort: Array[Int] = {

    @tailrec
    def insertInto(leftArray: Array[Int], rightArray: Array[Int], intruder: Int): Array[Int] = {
      if (rightArray.isEmpty) leftArray :+ intruder
      else {
        if (intruder <= rightArray.head) (leftArray :+ intruder) ++ rightArray
        else insertInto(leftArray :+ rightArray.head, rightArray.tail, intruder)
      }
    }

    @tailrec
    def createSorted(sortedArray: Array[Int], oldArray: Array[Int]): Array[Int] = {
      if (oldArray.isEmpty) sortedArray
      else createSorted(insertInto(Array[Int](), sortedArray, oldArray.head), oldArray.tail)
    }

    createSorted(Array[Int](), array)
  }

  def shellSort: Array[Int] = ???

  def mergeSort: Array[Int] = ???

  def quickSort: Array[Int] = ???


}
