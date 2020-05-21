package donat.com.manager

import donat.com.myArray.MyArray

class ArrayManager(size: Int, unique: Boolean = false, range: Int = 1000) {
  val myArray = MyArray(size, unique, range)

  def showMyArray: Unit = {
    println(s"Original array. Size: ${myArray.array.length} - Unique: ${myArray.unique} - Range: ${myArray.range}")
    println("-------------")
  }

  private def schuffle(map: Map[String, Long]): Map[String, Long] = {
    val t0 = System.nanoTime()
    val schuffledArray: Array[Int] = myArray.schuffle
    val t1 = System.nanoTime()
    val elapsedTime = t1 - t0
    if (!myArray.isItSorted(schuffledArray)) map + ("Schuffle" -> elapsedTime)
    else map + ("Schuffle - not succes" -> -1)
  }

  private def sortBySelection(map: Map[String, Long]): Map[String, Long] = {
    val t0 = System.nanoTime()
    val sortedArray: Array[Int] = myArray.selectionSort
    val t1 = System.nanoTime()
    val elapsedTime = t1 - t0
    if (myArray.isItSorted(sortedArray)) map + ("Sort By Selection" -> elapsedTime)
    else map + ("Sort By Selection - not succes" -> -1)
  }

  private def sortByInsertion(map: Map[String, Long]): Map[String, Long] = {
    val t0 = System.nanoTime()
    val sortedArray: Array[Int] = myArray.insertionSort
    val t1 = System.nanoTime()
    val elapsedTime = t1 - t0
    if (myArray.isItSorted(sortedArray)) map + ("Sort By Insertion" -> elapsedTime)
    else map + ("Sort By Insertion - not succes" -> -1)
  }

  def measureAlgorithmics: Unit = {
    val afterSchuffle = schuffle(Map[String, Long]())
    val afterSortBySelection = sortBySelection(afterSchuffle)
    val afterSortByInsertion = sortByInsertion(afterSortBySelection)

    afterSortByInsertion.foreach(m => println(s"${m._1}: elapsed time: ${m._2} ns"))
  }
}
