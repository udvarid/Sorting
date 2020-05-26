package donat.com.myArray

import scala.annotation.tailrec
import scala.util.Random

case class MyArray(size: Int, unique: Boolean, var range: Int) {

  val array: Array[Int] = initArray

  val swap: (Int, Int, Array[Int]) => Unit = (x, y, array) => {
    val puff = array(x)
    array(x) = array(y)
    array(y) = puff
  }

  private def initArray: Array[Int] = {
    val arrayInit: Array[Int] = new Array[Int](size)

    if (unique && range < size) range = size

    if (!unique) for (i <- 0 until size) arrayInit(i) = Random.nextInt(range)
    else {
      @tailrec
      def fillMySet(aSet: Set[Int]): Set[Int] = {
        if (aSet.size == size) aSet
        else fillMySet(aSet + Random.nextInt(range))
      }

      return fillMySet(Set[Int]()).toArray
    }

    arrayInit
  }

  def isItSorted(arrayToCheck: Array[Int]): Boolean = {
    for (i <- 0 until arrayToCheck.length - 1) {
      if (arrayToCheck(i) > arrayToCheck(i + 1)) return false
    }
    true
  }

  def schuffleOld: Array[Int] = {
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

  def schuffle: Array[Int] = {
    val newArray: Array[Int] = array map (identity)
    for (i <- 1 until newArray.length) {
      val exchangeIndex: Int = Random.nextInt(i)
      val exchangeValue: Int = newArray(exchangeIndex)
      newArray(exchangeIndex) = newArray(i)
      newArray(i) = exchangeValue
    }
    newArray
  }

  def selectionSortOld: Array[Int] = {

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

  def selectionSort: Array[Int] = {
    val newArray: Array[Int] = array map (identity)

    for (i <- 0 until newArray.length - 1) {
      var min: Int = Int.MaxValue
      var index: Int = 0
      for (j <- i until newArray.length) {
        if (newArray(j) <= min) {
          min = newArray(j)
          index = j
        }
      }
      newArray(index) = newArray(i)
      newArray(i) = min
    }

    newArray
  }

  def insertionSortOld: Array[Int] = {

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

  def insertionSort(shell: Boolean = false): Array[Int] = {
    val newArray: Array[Int] = array map identity

    var step: Int = {
      @tailrec
      def calcStep(stepValue: Int): Int = {
        if (stepValue >= newArray.length / 3) stepValue
        else calcStep(stepValue * 3 + 1)
      }

      if (!shell) 1
      else {
        calcStep(1)
      }
    }

    while (step >= 1) {
      for (i <- step until newArray.length) {
        var openDoor: Boolean = true
        var j = i - step
        while (j >= 0 && openDoor) {
          if (newArray(j) > newArray(j + step)) swap(j, j + step, newArray)
          else openDoor = false
          j = j - step
        }
      }
      step = step / 3
    }

    newArray
  }

  def mergeSort: Array[Int] = {
    val newArray: Array[Int] = array map identity

    def sortIt(s: Int, e: Int): Unit = {
      if (s < e) {
        val center = (s + e) / 2
        sortIt(s, center)
        sortIt(center + 1, e)
        merge(s, center, e)
      }
    }

    def merge(s: Int, m: Int, e: Int): Unit = {

      val size = e - s + 1
      val pufArray: Array[Int] = new Array[Int](size)
      var indA: Int = s
      var indB: Int = m + 1
      var indPuf: Int = 0

      while (indA <= m && indB <= e) {
        if (newArray(indA) <= newArray(indB)) {
          pufArray(indPuf) = newArray(indA)
          indA += 1
        } else {
          pufArray(indPuf) = newArray(indB)
          indB += 1
        }
        indPuf += 1
      }

      while (indA <= m) {
        pufArray(indPuf) = newArray(indA)
        indA += 1
        indPuf += 1
      }

      while (indB <= e) {
        pufArray(indPuf) = newArray(indB)
        indB += 1
        indPuf += 1
      }

      var indFill: Int = s
      for (i <- pufArray.indices) {
        newArray(indFill) = pufArray(i)
        indFill += 1
      }


    }

    sortIt(0, newArray.length - 1)

    newArray
  }

  def quickSort: Array[Int] = ???


}
