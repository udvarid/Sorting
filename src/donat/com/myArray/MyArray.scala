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

  def insertionSort: Array[Int] = {
    val newArray: Array[Int] = array map (identity)

    val swap: Int => Unit = x => {
      val puff = newArray(x)
      newArray(x) = newArray(x + 1)
      newArray(x + 1) = puff
    }

    for (i <- 1 until newArray.length) {
      var openDoor: Boolean = true
      var j = i - 1
      while (j >= 0 && openDoor) {
        if (newArray(j) > newArray(j + 1)) swap(j)
        else openDoor = false
        j = j - 1
      }
    }
    newArray
  }

  def shellSort: Array[Int] = {

    val newArray: Array[Int] = array map (identity)


    newArray

    //    public static void sort(Comparable[] a)
    //    {
    //      int N = a.length;
    //      int h = 1;
    //      while (h < N/3) h = 3*h + 1; // 1, 4, 13, 40, 121, 364, ...
    //      while (h >= 1)
    //      { // h-sort the array.
    //        for (int i = h; i < N; i++)
    //        {
    //          for (int j = i; j >= h && less(a[j], a[j-h]); j -= h)
    //          exch(a, j, j-h);
    //        }
    //
    //        h = h/3;
    //      }
    //    }
    //
  }

  def mergeSort: Array[Int] = ???

  def quickSort: Array[Int] = ???


}
