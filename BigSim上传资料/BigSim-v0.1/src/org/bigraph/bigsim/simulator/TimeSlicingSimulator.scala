package org.bigraph.bigsim.simulator

/**
 * @author liangwei
 * version 0.1
 */

import scala.collection.mutable.Queue
import scala.collection.mutable.Map
import scala.collection.mutable.Set
import scala.collection.immutable.TreeMap
import scala.util.Random
import java.io._
import org.bigraph.bigsim._
import org.bigraph.bigsim.utils.GlobalCfg
import org.bigraph.bigsim.model._
import org.bigraph.bigsim.BRS.Match
import org.bigraph.bigsim.BRS.Graph
import org.bigraph.bigsim.BRS.Vertex
import org.bigraph.bigsim.data.Data
import org.bigraph.bigsim.utils.Graphviz

object TimeSlicingSimulator {

  var matchDiscard: Set[Match] = Set();

  def matchMarkDelete(m: Match): Unit = {
    assert(m != null);
    matchDiscard.add(m);
  }

  def matchGC: Unit = {
    matchDiscard.clear();
  }
}

class TimeSlicingSimulator(b: Bigraph) extends Simulator {

  var v: Vertex = new Vertex(b, null, null);
  var g: Graph = new Graph(v);
  var states: Queue[Tuple2[Double, Vertex]] = Queue();
  var simQueue: TreeMap[Double, Queue[Match]] = TreeMap();
  var reactNodes: Set[String] = Set();

  var steps: Int = 0;
  var checked: Map[Long, Boolean] = Map();

  var path: List[String] = List();
  var variables: List[String] = List();
  var pathColor: String = Graphviz.getColor

  def simulate: Unit = {
    // add the initial agent to the simQueue
    states += ((0, v))

    if (b == null || b.root == null) {
      println("time slicing simulator::simulate(): null");
      return ;
    } else {
      // keep simulate until the end
      while (step()) {
        Simulator.matchGC
      };
      report()
      TimeSlicingSimulator.matchGC;
    }
  }

  def report() {
    GlobalCfg.node = false
    if (GlobalCfg.pathOutput != "" && GlobalCfg.outputPath) {
      var writer: Writer = new FileWriter(GlobalCfg.pathOutput, GlobalCfg.append);
      writer.write(GlobalCfg.curLoop + "{\n");
      writer.write(path.mkString("\n"));
      writer.write("\n}\n");
      writer.close();
    }
    GlobalCfg.node = true
    if (GlobalCfg.dataOutput != "" && GlobalCfg.outputData) {
      var writer: Writer = new FileWriter(GlobalCfg.dataOutput, GlobalCfg.append);
      writer.write(GlobalCfg.curLoop + "{\n");
      writer.write(variables.mkString("\n"));
      writer.write("\n}\n");
      writer.close();
    }
    GlobalCfg.append = true
  }

  def step(): Boolean = {

    /**
     * if meet max system clock, simulation stop.
     */
    if (GlobalCfg.SysClk > GlobalCfg.maxSysClk) {
      println("sim::step Interrupted!  Reached maximum SysClk: " + GlobalCfg.maxSysClk);
      return false;
    }

    /**
     * 0: update
     * If sim queue contains reactions happen at this time,
     * try match and apply match.
     */
    update()

    /**
     * 1: add match
     */
    if (!addMatch()) {
      return false
    }

    /**
     * 2: update if current match doesn't need reaction time
     * If sim queue contains reactions happen at this time,
     * try match and apply match.
     */
    update()

    TimeSlicingSimulator.matchGC;
    // update the system clk
    GlobalCfg.SysClk = GlobalCfg.SysClk + GlobalCfg.SysClkIncr
    Data.update("SysClk", GlobalCfg.SysClk.toString)
    true;
  }

  /**
   * update
   * update matches once the system clock meets
   */
  def update() {

    if (simQueue.contains(GlobalCfg.SysClk)
      && !simQueue.get(GlobalCfg.SysClk).isEmpty) {

      if (GlobalCfg.verbose)
        println("Update-----System Clock now:" + GlobalCfg.SysClk)
        
      // apply these matches
      var reactList: Queue[Match] = simQueue.getOrElse(GlobalCfg.SysClk, Queue())
      // match and find match and apply match!!!
      var v: Vertex = states.last._2
      var curBigraph = v.bigraph
      var curRRs: Set[ReactionRule] = Set()
      // record the cond
      var rules: Map[String, List[String]] = Map()
      var conds: List[String] = List()

      while (reactList.size > 0) {
        var tm = reactList.dequeue
        var matches: Set[Match] = curBigraph.findMatchesOfRR(tm.rule)
        if (matches != null) {
          var matched: Boolean = false
          matches.map(m => {
            if (GlobalCfg.verbose) {
              println(m.rule.name + "," + m.getReactNodes + "," +
                tm.getReactNodes + "matched:" + matched)
            }

            if (!matched && m.getReactNodes.equals(tm.getReactNodes)) {
              var nb: Bigraph = curBigraph.applyMatch(m)
              /**
               * update a reaction rule data model
               */
              m.rule.update(tm)
              /**
               * update agent data with clock
               */
              Data.updateDataCalcsWithClk(tm.RRIncr.toString)

              curRRs += m.rule

              if (rules.contains(m.rule.name)) {
                rules.put(m.rule.name, rules.getOrElse(m.rule.name, List()).++(m.reactNodes.toList))
              } else {
                rules.put(m.rule.name, m.reactNodes.toList)
              }
              if (!m.rule.getConds.equals("")) {
                conds = conds.:+(m.rule.getConds)
              }

              if (GlobalCfg.verbose) {
                println("-----react nodes before:" + reactNodes)
              }

              reactNodes = reactNodes.filter(!m.reactNodes.contains(_))
              if (GlobalCfg.verbose) {
                println("-----reaction nodes rm:" + m.reactNodes)
                println("-----react nodes after:" + reactNodes)
              }
              
              matched = true
              if (nb.root == null) {
                nb.root = new Nil();
              }
              if (GlobalCfg.verbose) {
                println("middle result match RR " + tm.rule.name + " : " + nb.root.toString)
                println("middle result of variables: " + Data.getValues(","))
              }
              curBigraph = nb
            }
          })
        }
      }

      if (curBigraph != null && curRRs != null) {
        var nv = new Vertex(curBigraph, v, curRRs, true)
        nv.sysClk = GlobalCfg.SysClk

        if (GlobalCfg.outputGraph) {
          dot += "N_" + formatHash(nv.hash) +
            "[ " + "label=\"N_" + formatHash(nv.hash) + "\"];\n";
          dot += " N_" + formatHash(v.hash) +
            " -> N_" + formatHash(nv.hash) + "[ color = " + pathColor + " label = \"SysClk:" + GlobalCfg.SysClk +
            "\n" + rules.mkString(",") + "\"];\n"
        }
        if (GlobalCfg.outputPath) {
          path = path.:+(nv.bigraph.root.toString)
        }
        if (GlobalCfg.outputData) {
          variables = variables.:+(nv.variables)
        }

        if (g.lut.contains(nv.hash)) {
          nv = g.lut(nv.hash);
          nv.addParents(v)
        } else {
          g.add(nv);
        }

        v.addTargets(curRRs, nv);
        states += (GlobalCfg.SysClk -> nv)
        if (GlobalCfg.printMode) {
          print("SysClk:" + GlobalCfg.SysClk + "\t")
          printf("%s:%s\n", "N_" + Math.abs(nv.hash), nv.bigraph.root.toString);
          //println(nv.variables)
        }
      }

      // finally, delete it!
      simQueue = simQueue.-(GlobalCfg.SysClk)
    }
  }

  def addMatch(): Boolean = {
    var v: Vertex = states.last._2
    steps += 1;
    var b: Bigraph = v.bigraph;
    var matches: Set[Match] = b.findMatches;

    if (steps >= GlobalCfg.maxSteps) {
      println("sim::step Interrupted!  Reached maximum steps: " + GlobalCfg.maxSteps);
      return false;
    }

    /**
     * if current model can not find matches and the sim queue is empty,
     * then the simulation is over.
     * Abandon! not right
     */
    /*
    if (matches.isEmpty && simQueue.isEmpty) {
      println("sim::step Complete!");
      report(steps);
      TimeSlicingSimulator.matchGC;
      return false;
    } 
    */

    /**
     * if there is no match, but the sim queue is not emputy
     *
     * if (matches.isEmpty && !simQueue.isEmpty) {
     * println("Current Agent Can not match rules, jump to time:" + simQueue.firstKey)
     * GlobalCfg.SysClk = simQueue.firstKey
     * return true
     * }
     */

    //println("Add match-----matches size: " + matches.size)

    /**
     * If a reaction rule is not random and not conflict,
     * it must happen when it is matched.
     */
    matches.map(m => {
      if (GlobalCfg.verbose) {
        println("All match:" + m.rule.name + "\tcond:" +
          m.rule.conds + "\treactNodes:" + m.reactNodes)
      }
          
      val conflict = m.conflict(reactNodes.toList)
      if (!conflict) {
        //if (!conflict && !m.rule.random) {
        val RRIncr = m.rule.getRRIncr
        var reactTime = GlobalCfg.SysClk + RRIncr
        m.RRIncr = RRIncr
        var queue: Queue[Match] = null
        if (simQueue.contains(reactTime)) {
          queue = simQueue(reactTime)
          queue += m
        } else {
          queue = Queue(m)
        }
        simQueue += reactTime -> queue
        reactNodes ++= m.reactNodes
        if (GlobalCfg.verbose) {
          println("add match: " + m.rule.name + "\treact nodes:" +
            m.reactNodes + "\treact time:" + reactTime)
        }
        //matches -= m
      } else if (conflict) {
        //matches -= m
      }
    })
    return true;
  }

  def formatHash(hash: Int): String = {
    if (hash < 0) "_" + hash.abs;
    else hash.toString;
  }

  def dumpDotForward(dot: String): String = {
    var out: String = "";
    out += "digraph reaction_graph {\n";
    out += "   rankdir=LR;\n";
    out += "   Node [shape = circle];\n";
    out += "   BigSim_Report [shape = parallelogram color = aliceblue style=filled label=\"BigSim\nReport\"];\n"
    out += "BigSim_Report -> N_" + formatHash(g.root.hash) + "[color = aliceblue label = \"" +
      Data.getWeightExpr + "=" +
      Data.getReport + "\n" + //Data.getValues(",") +
      "\"];\n";
    out += " N_" + formatHash(g.root.hash) + "\n" + " [shape=circle, color=lightblue2, style=filled];\n";
    out += dot;
    out += "}\n";
    if (GlobalCfg.graphOutput != "" && GlobalCfg.outputGraph) {
      var file: File = new File(GlobalCfg.graphOutput);
      var writer: Writer = new FileWriter(file);
      writer.write(out);
      writer.flush;
      writer.close();
    }
    out;
  }
}
