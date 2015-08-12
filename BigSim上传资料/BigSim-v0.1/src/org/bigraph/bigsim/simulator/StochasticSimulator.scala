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
import cern.jet.random.engine.RandomEngine
import cern.jet.random.Uniform
import cern.jet.random.Exponential
import org.bigraph.bigsim.data.DataModel
import org.bigraph.bigsim.model._
import org.bigraph.bigsim.BRS._
import org.bigraph.bigsim.BRS.Graph
import org.bigraph.bigsim.BRS.Vertex
import org.bigraph.bigsim.data.Data

object StochasticSimulator {

  var matchDiscard: Set[Match] = Set();

  def matchMarkDelete(m: Match): Unit = {
    assert(m != null);
    matchDiscard.add(m);
  }

  def matchGC: Unit = {
    matchDiscard.clear();
  }

  def InversionSampling(ruleActivities: Map[Int, Double]): Int = {
    var cumulate: Double = 0
    ruleActivities.map(r => {
      cumulate += r._2
    })
    val re: RandomEngine = RandomEngine.makeDefault
    val un: Uniform = new Uniform(0, cumulate, re)
    val r: Double = un.nextDouble()
    cumulate = 0
    var cdf: TreeMap[Int, Double] = new TreeMap()
    ruleActivities.map(rule => {
      cumulate += rule._2
      if (cumulate >= r)
        return rule._1
    })
    ruleActivities.keys.toList(0)
  }
}

class StochasticSimulator(b: Bigraph) extends Simulator {
  var v: Vertex = new Vertex(b, null, null);
  var g: Graph = new Graph(v);
  var states: Queue[Tuple2[Double, Vertex]] = Queue();
  var reactNodes: Set[String] = Set();

  var steps: Int = 0;
  var checked: Map[Long, Boolean] = Map();

  def simulate: Unit = {
    /**
     * Store the simPath and simRules
     */
    var simPath: Queue[Term] = Queue()
    var simRules: Set[String] = Set()

    /**
     * 0. Initialization:
     * Initialize the simulation state
     * t = 0
     * M(a,R) = match(a,R)
     * αR = |M(a,R)|*Pr
     * α=αR1+αR2+...+αRn
     * If α==0, simulation end
     */
    // add the initial agent to the simQueue
    var curv: Vertex = v
    var curb: Bigraph = v.bigraph
    simPath += curb.root
    states += ((0, curv))
    var matchMap: Map[Int, Set[Match]] = Map()
    var systemActivity: Double = 0.0
    var ruleActivities: Map[Int, Double] = Map()
    var time: Double = 0.0

    var ruleIndex: Int = 0
    println("rule size:" + b.rules.size)
    b.rules.map(r => {
      ruleIndex += 1
      println(r.name);
      var matches: Set[Match] = curb.findMatchesOfRR(r)
      matchMap += ruleIndex -> matches

      println("ruleIndex:" + ruleIndex + " matches:" + matches.size)
      //calculate rule activities
      val ruleActivity = r.rate * matches.size
      ruleActivities += ruleIndex -> ruleActivity

      //calculate system activity
      systemActivity += ruleActivity
    })

    while (systemActivity > 0) {
      /**
       * 1. Monte Carlo step:
       * Sample the following random values
       * R=Rand(Rule set, PPS Sampling of ruleActivity/systemActivity)
       * m=Rand(M(a,R), Uniform distribution of 1/M(a,R))
       */
      // sample rule
      val ru: Int = StochasticSimulator.InversionSampling(ruleActivities.filter(r => r._2 > 0).map(r => { r._1 -> r._2 / systemActivity }))
      println("which rule:" + ru + " match size:" + matchMap(ru).size)
      // sample match
      val re: RandomEngine = RandomEngine.makeDefault
      val un: Uniform = new Uniform(0, matchMap(ru).size - 1, re)
      val ma: Match = matchMap(ru).toList(un.nextInt())
      val ex: Exponential = new Exponential(systemActivity, re)
      val tIncr: Double = ex.nextDouble()
      println("time Incr:" + tIncr)

      /**
       * 2. Update:
       * Update the simulation state
       * perform reaction
       * time = time + tIncr
       * update set of matches
       * update rule activities
       * update system activity
       */
      var newb: Bigraph = curb.applyMatch(ma)
      simPath += newb.root
      simRules += ma.rule.name
      var newv: Vertex = null
      time += tIncr
      if (curb != null && newb != null) {
        /** update a reaction rule data model */
        ma.rule.update
        /** update agent data with clock */
        //DataModel.updateDataCalcsWithClk(tIncr.toString)
        if (newb.root == null)
          newb.root = new Nil();
        newv = new Vertex(newb, curv, ma.rule)
        newv.sysClk = time
        newv.parent = curv
        if (g.lut.contains(newv.hash)) {
          newv = g.lut(newv.hash);
          newv.addParents(curv)
        } else {
          g.add(newv);
        }
        curv.addTarget(newv, ma.rule);
        states += (time -> newv)
        if (GlobalCfg.printMode) {
          printf("%s:%s\n", "N_" + Math.abs(newv.hash), newv.bigraph.root.toString);
        }
      }
      // clean all the containers and update
      curv = newv
      curb = newb
      matchMap.empty
      systemActivity = 0.0
      ruleActivities.empty
      ruleIndex = 0
      b.rules.map(r => {
        ruleIndex += 1
        var matches: Set[Match] = curb.findMatchesOfRR(r)
        matchMap += ruleIndex -> matches
        println("ruleIndex:" + ruleIndex + " matches:" + matches.size)

        //calculate rule activities
        val ruleActivity = r.rate * matches.size
        ruleActivities += ruleIndex -> ruleActivity

        //calculate system activity
        systemActivity += ruleActivity
      })
    }

    dump(simPath, simRules.size)
    StochasticSimulator.matchGC;
  }

  def dump(simPath: Queue[Term], simRules: Int) {
    dumpDotForward("")
    //GlobalCfg.node = false
    if (GlobalCfg.pathOutput != "") {
      var out: String = "{\n"
      while (!simPath.isEmpty)
        out += simPath.dequeue.toString + "\n"
      out += "}" + (100 * simRules.toDouble / b.rules.size).toString + "%\n"
      println(out)
      var file: File = new File(GlobalCfg.pathOutput);
      var writer: Writer = new FileWriter(file);
      writer.write(out);
      writer.flush;
    }
    GlobalCfg.node = true
  }

  def formatHash(hash: Int): String = {
    if (hash < 0) "_" + hash.abs;
    else hash.toString;
  }

  def dumpDotForward(dot:String): String = {
    var out: String = "";
    out += "digraph reaction_graph {\n";
    out += "   rankdir=LR;\n";
    out += "   Node [shape = circle];\n";
    out += "   BigSim_Report [shape = parallelogram color = aliceblue style=filled label=\"BigSim\nReport\"];\n"
    out += "BigSim_Report -> N_" + formatHash(g.root.hash) + "[color = aliceblue label = \"" +
      Data.getWeightExpr + "=" +
      Data.getReport + "\n" +
      Data.getValues(",") + "\"];\n";
    out += " N_" + formatHash(g.root.hash) + "\n" + " [shape=circle, color=lightblue2, style=filled];\n";
    g.lut.values.map(x => {
      var rr: String = "root";
      var dc: String = "";
      if (x.terminal) {
        dc = "shape = doublecircle, color=lightblue2, style=filled, ";
      }
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
          out += " N_" + formatHash(x.hash) + " -> N_" + formatHash(y._1.hash) + "[ label = \"" + rr + "\n" + y._1.variables + "\"];\n"
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

}
