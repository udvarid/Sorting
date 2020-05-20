package donat.com

import donat.com.manager.ArrayManager

object Starter extends App {
  val arrayManager = new ArrayManager(20)

  arrayManager.showMyArray

  arrayManager.sortBySelection

}
