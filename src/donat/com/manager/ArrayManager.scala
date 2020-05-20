package donat.com.manager

import donat.com.myArray.MyArray

class ArrayManager(size: Int, unique: Boolean = false, range: Int = 1000) {
  val myArray = MyArray(size, unique, range)

  def showMyArray: Unit = {
    println(s"Original array. Size: ${myArray.array.length} - Unique: ${myArray.unique} - Range: ${myArray.range}")
    println(myArray.array.mkString(" "))
    println("-------------")
  }

  def sortBySelection: Unit = {
    val t0 = System.nanoTime()
    val sortedArray: Array[Int] = myArray.selectionSort
    val t1 = System.nanoTime()
    println("Sort by Selection  -  Elapsed time: " + (t1 - t0) + "ns")
    println(sortedArray.mkString(" "))
    println("-------------")
  }
}
