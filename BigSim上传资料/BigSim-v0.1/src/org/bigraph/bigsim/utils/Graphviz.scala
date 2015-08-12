package org.bigraph.bigsim.utils

import scala.collection.mutable.Map

object Graphviz {

  val colors: List[String] = List("coral1", "purple", "darkgoldenrod1", "chartreuse1",
    "darkorange1", "aquamarine1", "aliceblue", "blueviolet", "darkslategray1", "crimson",
    "brown1", "orange1", "gold1", "darkgreen", "cyan1", "blue1", "darkviolet1",
    "dimgray", "darksalmon", "chocolate1", "orangered1", "goldenrod1", "darkturquoise",
    "blueviolet", "darkorchild1", "deeppink1", "greenyellow", "forestgreen", "lightcyan1", "magenta1")

  def getColor(): String = {
    colors(GlobalCfg.curLoop % colors.size)
  }
}