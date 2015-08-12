package org.bigraph.bigsim.BRS

import java.io.File
import java.io.FileWriter
import java.io.Writer
import scala.collection.mutable.Map
import scala.collection.mutable.Set
import scala.collection.mutable.Stack
import scala.xml.XML
import org.bigraph.bigsim.data.DataModel
import org.bigraph.bigsim.model.ReactionRule
import org.bigraph.bigsim.strategy.ParseRules
import org.bigraph.bigsim.strategy.ParseXML
import org.bigraph.bigsim.utils.GlobalCfg
import org.bigraph.bigsim.data.Data

class Graph(init: Vertex) {
  val root: Vertex = init;
  //lut: look up table
  val lut: Map[Int, Vertex] = Map();
  if (root != null) lut(root.hash) = root;

  def add(v: Vertex): Unit = {
    lut(v.hash) = v;
  }

  def size: Int = lut.size;

  def backTrace(v: Vertex): String = {
    //todo
    var vertex: Vertex = v;
    var i: Int = 0;
    var here: String = "";
    while (vertex != null) {
      if (i == 0) here += "   <- *** VIOLATION *** \n";
      // FIXME: 字符串连接顺序可能有误，需要反过来？
      here += "#" + i + " " + vertex.bigraph.root.toString() + "\n";
      if (vertex.reactionRule == null) {
        here += " >> (root)\n";
      } else {
        here += " >> " + vertex.reactionRule.toString + "\n";
      }
      i += 1;
      vertex = vertex.parent;
    }
    here;
  }

  /**
   * get Pathes has interested pattern
   */
  def getPathsHasInterestPatterns: Set[Stack[Vertex]] = {
    var result: Set[Vertex] = Set()
    var allPaths: Set[Stack[Vertex]] = getAllPaths
    println("All pathes size is: " + allPaths.size)
    var selectedPaths: Set[Stack[Vertex]] = Set() //这里用于存储筛选掉之后的所有path

    allPaths.map(ite => {
      var pathStack: Stack[Vertex] = Stack()
      var size: Int = ite.size

      var ruleName: Array[String] = new Array[String](size)
      for (i <- 0 to size - 1) {
        if (ite.head.reactionRule != null) {
          ruleName(i) = ite.head.reactionRule.name
        } else {
          ruleName(i) = null
        }
        pathStack.push(ite.head)
        ite.pop
      }

      var containsInteresPattern: Boolean = false
      var interesRules: Set[String] = ParseXML.getNamesOfIntrestedPatterns(root.bigraph, XML.load(GlobalCfg.patternFile))
      interesRules.map(it => {
        if (ruleName.contains(it)) {
          containsInteresPattern = true
        }

      })

      if (containsInteresPattern) {
        pathStack.map(itps => {
          ite.push(itps)
        })
      }
      if (ite != null) {
        selectedPaths += ite
      }
      pathStack.clear
    })
    selectedPaths
  }
  /*
	 * 找一个数据结构，存储根据Vertexs中属性terminal为true的找出来每一条路径。
	 * 中间，根绝策略筛选路径。
	 * 筛选好的所有路径放入到一个set中去,返回的路径是以dot文件的形式，所以
	 */
  def findPathsByStrategy(rules: Set[ReactionRule]): Set[Stack[Vertex]] = {
    //这里，到底提供那种strategy，可以再global里面定义
    var result: Set[Vertex] = Set()
    var allPathes: Set[Stack[Vertex]] = getAllPaths
    var selectedPathes: Set[Stack[Vertex]] = Set() //这里用于存储筛选掉之后的所有path

    var dcuMaps: Map[String, Set[String]] = ParseRules.getAllDCUs(rules)
    var dpuMaps: Map[String, Set[String]] = ParseRules.getAllDPUs(rules)
    var sameDefRules: Map[String, Set[String]] = ParseRules.getAllRulesWithSameDef(rules)

    allPathes.map(ite => {
      var pathStack: Stack[Vertex] = Stack()
      var size: Int = ite.size
      var ruleName: Array[String] = new Array[String](size)

      for (i <- 0 to size - 1) {
        if (ite.head.reactionRule != null) {
          ruleName(i) = ite.head.reactionRule.name
        } else {
          ruleName(i) = null
        }
        pathStack.push(ite.head)
        ite.pop
      }

      var containsDU: Boolean = false

      //保证du的定义是清纯的，其实可以直接找路径中是否存在du对，已经概括了其中的情况了。
      for (i <- 0 to (size - 2) if containsDU == false) {
        for (j <- (i + 1) to (size - 1) if containsDU == false) {
          if (ruleName(i) != null) {
            if (GlobalCfg.allUses) {
              if (dcuMaps(ruleName(i)).contains(ruleName(j))) {
                for (k <- (j + 1) to (size - 1) if containsDU == false) {
                  if (dpuMaps(ruleName(i)).contains(ruleName(k))) {
                    containsDU = true
                  }
                }
              } else if (dpuMaps(ruleName(i)).contains(ruleName(j))) {
                for (k <- (j + 1) to (size - 1) if containsDU == false) {
                  if (dcuMaps(ruleName(i)).contains(ruleName(k))) {
                    containsDU = true
                  }
                }
              }
            } else if (GlobalCfg.allDefs) {
              if (dcuMaps(ruleName(i)).contains(ruleName(j)) || dpuMaps(ruleName(i)).contains(ruleName(j))) {
                containsDU = true
              }
            }

          }
        }
      }

      if (containsDU) {
        pathStack.map(itps => {
          ite.push(itps)
        })
      }
      if (ite != null) {
        selectedPathes += ite
      }
      pathStack.clear
    })

    selectedPathes
  }

  /**
   * 使用一个stack来存储一条路径。
   * 然后把所有路径放到一个set里面去。
   */
  def getAllPaths: Set[Stack[Vertex]] = {
    var allPathSet: Set[Stack[Vertex]] = Set()
    println("Vertexs size is : " + lut.values.size)

    lut.values.map(ite => {
      if (ite.parent != null) {
        var lastVertex: Boolean = true
        lut.values.map(it => {
          if (it.parent != null && ite.hash == it.parent.hash) {
            lastVertex = false
          }
        })
        if (lastVertex == true) {
          var pathStack: Stack[Vertex] = Stack()
          var tempVertex: Vertex = ite
          pathStack.push(tempVertex)
          while (tempVertex.parent != null) {
            pathStack.push(tempVertex.parent)
            tempVertex = tempVertex.parent
          }
          allPathSet.add(pathStack)
        }
      }
    })
    allPathSet
  }

  def dumpPaths(): String = {
    var allRulesNum: Double = root.bigraph.rules.size
    var defPathMap: Map[String, Set[Int]] = Map()

    var paths: Set[Stack[Vertex]] = Set()
    if (GlobalCfg.allUses || GlobalCfg.allDefs) {
      paths ++= findPathsByStrategy(root.bigraph.rules)
    } else if (GlobalCfg.checkInterestPattern) {
      paths ++= getPathsHasInterestPatterns
    } else {
      //pathes ++= getAllPathes
      paths ++= getAllPathss
    }

    var out: String = ""
    var pathNum: Int = -1
    paths.map(ite => {
      var ruleNameSet: Set[String] = Set()
      ite.map(itN => {
        if (itN.reactionRule != null) {
          ruleNameSet.add(itN.reactionRule.name)
        }
      })
      var ruleNameSize: Double = ruleNameSet.size
      var ruleCoverage: String = (math floor (ruleNameSize / allRulesNum) * 100).toString + "%"

      if (ite.size != 0) {
        pathNum += 1

        if (GlobalCfg.allDefs) {
          // 得到每一个def和路径的映射，def1{0， 2， 4， 8}；
          root.bigraph.rules.map(ruleIte => {
            if (ruleNameSet.contains(ruleIte.name)) {
              ruleIte.defTerm.map(defTermIte => {
                if (defPathMap.contains(defTermIte.toString)) {
                  defPathMap(defTermIte.toString).add(pathNum)
                } else {
                  defPathMap += (defTermIte.toString -> Set(pathNum))
                }
              })
            }
          })
        }

        out += pathNum + "{\n"
        //中间输出一个stack里面的Vertex的model，每一个model中间用分号分割
        while (ite.size > 0) {
          var rootStr: String = ite.head.bigraph.root.toString
          if (rootStr.charAt(0) != '(') {
            out += rootStr
          } else {
            out += rootStr.substring(1, rootStr.size - 1)
          }
          out += ";\n"
          ite.pop
        }
        out += "}" + ruleCoverage + "\n"
        println("pathNum: "+pathNum)
      }

    })
    if (GlobalCfg.pathOutput != "") {
      var file: File = new File(GlobalCfg.pathOutput);
      var writer: Writer = new FileWriter(file);
      writer.write(out);
      writer.flush;
      writer.close()
    }

    if (GlobalCfg.defPathMapFile != "") {
      var defPath: String = ""
      defPathMap.map(ite => {
        defPath += ite._1 + "{"
        var tempdefPath: String = ""
        ite._2.map(it => {
          tempdefPath += it + ", "
        })
        defPath += tempdefPath.substring(0, tempdefPath.size - 2)
        defPath += "}\n"
      })

      var file: File = new File(GlobalCfg.defPathMapFile);
      var writer: Writer = new FileWriter(file);
      writer.write(defPath);
      writer.flush;
    }
    println(out)
    out
  }

  def dumpDotForward: String = {
    //if (GlobalCfg.graphOutput == "") return "";
    var out: String = "";
    out += "digraph reaction_graph {\n";
    out += "   rankdir=LR;\n";
    out += "   Node [shape = circle];\n";
    out += "   BigSim_Report [shape = parallelogram color = aliceblue style=filled label=\"BigSim\nReport\"];\n"
    out += "BigSim_Report -> N_" + formatHash(root.hash) + "[color = aliceblue label = \"";
    if (!Data.getWeightExpr.equals("wExpr="))
      out += Data.getWeightExpr + "=" + Data.getReport + "\n";
    out += Data.getValues(",") + "\"];\n";
    out += " N_" + formatHash(root.hash) + "\n" + " [shape=circle, color=lightblue2, style=filled];\n";
    lut.values.map(x => {
      var rr: String = "root";
      var dc: String = "";

      if (x.terminal) {
        dc = "shape = doublecircle, color=lightblue2, style=filled, ";
      }
      //out += "N_" + formatHash(x.hash) + "[ " + dc + "label=\"N_" + formatHash(x.hash) + "\n" + x.variables + "\"];\n";
      out += "N_" + formatHash(x.hash) + "[ " + dc + "label=\"N_" + formatHash(x.hash) + "\"];\n";
      x.target.map(y => {
        rr = "?";
        if (y._2 != null)
          rr = y._2.name;

        if (y._1 != null) {
          rr = rr + "\nSystem Clock: " + y._1.sysClk
          if (GlobalCfg.checkData && y._2.conds.size != 0)
            rr = rr + "\nCond:" + y._2.getConds
          if (GlobalCfg.checkHMM && y._2.hmms.size != 0)
            rr = rr + "\nHMM:" + y._2.getHMM
          out += " N_" + formatHash(x.hash) + " -> N_" + formatHash(y._1.hash) + "[ label = \"" + rr + "\"];\n"
        }
      });

    });
    out += "}\n";
    if (GlobalCfg.graphOutput != "") {
      var file: File = new File(GlobalCfg.graphOutput);
      var writer: Writer = new FileWriter(file);
      writer.write(out);
      writer.flush;
    }
    out;
  }

  def formatHash(hash: Int): String = {
    if (hash < 0) "_" + hash.abs;
    else hash.toString;
  }

  def getAllPathss: Set[Stack[Vertex]] = {
    var allPathSet: Set[Stack[Vertex]] = Set()
    var allPathString: Set[String] = Set()
    root.terminal = true
    addPaths("", allPathString, root)

    allPathString.map(path => {
      var p: Stack[Vertex] = Stack()
      var Vertexs = path.split("#")
      for (i <- 0 to Vertexs.size - 1) {
        if (!Vertexs(Vertexs.size - 1 - i).contains("r_")) {
          var Vertex = lut.getOrElse(Vertexs(Vertexs.size - 1 - i).toInt, null)
          if (Vertex != null)
            p.push(Vertex)
          else
            println("error!")
        }
      }
      allPathSet.add(p)

    })

    allPathSet

  }

  var firstTime: Boolean = true
  //var pathFile: File = new File("Airport/paths/BusinessNormal.txt");
  //var writer: Writer = new FileWriter(pathFile);

  def addPaths(path: String, allPathString: Set[String], currentVertex: Vertex) {
    if (currentVertex.terminal && !firstTime) {
      
      println("allPathString: "+path + currentVertex.hash);
      allPathString.add(path + currentVertex.hash)
      //writer.write(path + currentVertex.hash + "\n");
      //writer.flush;
      return
    } else {
      firstTime = false
      currentVertex.target.map(target => {
        if (!path.contains(currentVertex.hash + "#" + target._2.name + "#" + target._1.hash)) {
          addPaths(path + currentVertex.hash + "#" + target._2.name + "#", allPathString, target._1)
//          println("path:"+path+"   "+currentVertex.hash + "#" + target._2.name + "#" + target._1.hash);
//          println(path + currentVertex.hash + "#" + target._2.name + "#"+"  "+ allPathString + "  "+target._1);
        }
      })
    }

  }

}
