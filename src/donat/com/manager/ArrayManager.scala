package donat.com.manager

import donat.com.myArray.MyArray

class ArrayManager(size: Int, unique: Boolean = false, range: Int = 1000) {
  val myArray: MyArray = MyArray(size, unique, range)

  var results: Map[String, Long] = Map[String, Long]()

  def showMyArray: Unit = {
    println(s"Original array. Size: ${myArray.array.length} - Unique: ${myArray.unique} - Range: ${myArray.range}")
    println("-------------")
  }

  private def schuffle: Unit = {
    val t0 = System.nanoTime()
    val schuffledArray: Array[Int] = myArray.schuffle
    val t1 = System.nanoTime()
    val elapsedTime = t1 - t0
    if (!myArray.isItSorted(schuffledArray)) results += ("Schuffle" -> elapsedTime)
    else results += ("Schuffle - not succes" -> -1)
  }

  private def sortBySelectionScala: Unit = {
    val t0 = System.nanoTime()
    val sortedArray: Array[Int] = myArray.selectionSortOld
    val t1 = System.nanoTime()
    val elapsedTime = t1 - t0
    if (myArray.isItSorted(sortedArray)) results += ("Sort by Selection, Scala style" -> elapsedTime)
    else results += ("Sort by Selection, Scala style - not succes" -> -1)
  }


  private def sortBySelection: Unit = {
    val t0 = System.nanoTime()
    val sortedArray: Array[Int] = myArray.selectionSort
    val t1 = System.nanoTime()
    val elapsedTime = t1 - t0
    if (myArray.isItSorted(sortedArray)) results += ("Sort by Selection" -> elapsedTime)
    else results += ("Sort by Selection - not succes" -> -1)
  }

  private def sortByInsertion: Unit = {
    val t0 = System.nanoTime()
    val sortedArray: Array[Int] = myArray.insertionSort
    val t1 = System.nanoTime()
    val elapsedTime = t1 - t0
    if (myArray.isItSorted(sortedArray)) results += ("Sort by Insertion" -> elapsedTime)
    else results += ("Sort by Insertion - not succes" -> -1)
  }

  def measureAlgorithmics: Unit = {

    schuffle
    //sortBySelectionScala
    sortBySelection
    sortByInsertion

    results.foreach(m => println(s"${m._1}: elapsed time: ${m._2} ns"))
  }
}
