package donat.com

import donat.com.manager.ArrayManager

object Starter extends App {
  val arrayManager = new ArrayManager(10000)

  arrayManager.showMyArray

  arrayManager.measureAlgorithmics

}
