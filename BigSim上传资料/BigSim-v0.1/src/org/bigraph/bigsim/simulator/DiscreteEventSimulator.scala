package org.bigraph.bigsim.simulator

import scala.collection.immutable.TreeMap
import scala.collection.mutable.Map
import scala.collection.mutable.Set
import scala.collection.mutable.Stack
import org.bigraph.bigsim.utils.GlobalCfg
import org.bigraph.bigsim.BRS.Graph
import org.bigraph.bigsim.model.ReactionRule
import org.bigraph.bigsim.BRS.Vertex
import org.bigraph.bigsim.data.DataModel
import org.bigraph.bigsim.utils.GlobalCfg
import scala.collection.immutable.Queue
import org.bigraph.bigsim.model.Bigraph
import org.bigraph.bigsim.BRS.Match
import org.bigraph.bigsim.model.Nil
import org.bigraph.bigsim.data.Data

object DiscreteEventSimulator {
  var matchDiscard: Set[Match] = Set();

  def matchMarkDelete(m: Match): Unit = {
    assert(m != null);
    matchDiscard.add(m);
  }

  def matchGC: Unit = {
    matchDiscard.clear();
  }
}

class DiscreteEventSimulator(b: Bigraph) extends Simulator {
  /** original graph */
  var v: Vertex = new Vertex(b, null, null);
  var g: Graph = new Graph(v);

  var forwardPath: Queue[Vertex] = Queue();
  var backwardPath: Stack[Vertex] = Stack();

  var forwardReactNodes: Set[String] = Set();
  var backwardReactNodes: Set[String] = Set();

  /** simulate steps */
  var steps: Int = 0;
  var checked: Map[Long, Boolean] = Map();

  def simulate: Unit = {
    if (b == null || b.root == null) {
      println("DiscreteEvent::simulate(): null");
      return ;
    } else {
      if (GlobalCfg.printMode) {
        printf("%s:%s\n", "N_" + Math.abs(v.hash), v.bigraph.root.toString);
        println(v.variables)
      }

      /** init the start state */
      forwardPath.enqueue(v);
      backwardPath.push(v);

      /** iterator the simulate step */
      while (backwordStep()) {
      }
      report;
      DiscreteEventSimulator.matchGC;
    }
  }

  def report(): String = {
    GlobalCfg.node = false
    if (GlobalCfg.pathOutput != "")
      g.dumpPaths
    GlobalCfg.node = true
    g.dumpDotForward
  }

  def backwordStep(): Boolean = {
    /** whether reach the max steps */
    if (steps >= GlobalCfg.maxSteps) {
      println("simulate::step Interrupted!  Reached maximum steps: " + GlobalCfg.maxSteps);
      report();
      return false;
    }
    // 待检测模型队列为空
    if (forwardPath.size == 0) {
      println("sim::step Complete!");
      report();
      DiscreteEventSimulator.matchGC;
      return false;
    }

    // 取出栈顶模型
    var v: Vertex = forwardPath.last;
    steps += 1;
    var step: Int = steps;
    var b: Bigraph = v.bigraph;

    // 模型使用规则进行匹配
    var matches: Set[Match] = b.findMatches;
    //matches.foreach(f => { f.reactNodes.foreach(println) })
    checked(v.hash) = true;
    if (matches.size == 0) { //没有可以匹配的规则，该节点为终结节点
      v.terminal = true;
      return false;
    }

    var simRRMap: TreeMap[String, Match] = TreeMap();
    var isFirst = true

    if (matches.size > 0) {

      // delete conflict react node
      matches.map(it => {
        var conflict = false
        it.reactNodes.map(rn => {
          println(rn)
          if (forwardReactNodes.contains(rn)) {
            conflict = true
          }
        })
        if (conflict) {
          matches -= it
        } else if (!it.rule.random) {
          var key = (GlobalCfg.SysClk + it.rule.getRRIncr).toString + "_" + scala.util.Random.nextInt(100000)
          while (simRRMap.contains(key)) {
            key = (GlobalCfg.SysClk + it.rule.getRRIncr).toString + "_" + scala.util.Random.nextInt(100000)
          }
          simRRMap += key -> it
          forwardReactNodes ++ it.reactNodes
          matches -= it
        }
      });

      // find one rule meets the condition
      if (matches.size > 0) {
        var randIndex = scala.util.Random.nextInt(matches.size)
        var curMatch = matches.toList(randIndex)

        var rr: ReactionRule = curMatch.rule;
        var key = (GlobalCfg.SysClk + rr.getRRIncr).toString + "_" + scala.util.Random.nextInt(100000)
        while (simRRMap.contains(key)) {
          key = (GlobalCfg.SysClk + rr.getRRIncr).toString + "_" + scala.util.Random.nextInt(100000)
        }

        if (isFirst || !curMatch.rule.random) {
          simRRMap += key -> curMatch
          forwardReactNodes ++ curMatch.reactNodes
        } else if (scala.util.Random.nextInt(2) == 1) {
          simRRMap += key -> curMatch
          forwardReactNodes ++ curMatch.reactNodes
        }
        matches -= curMatch
      }
      isFirst = false
    }

    var curBigraph = v.bigraph
    var curRR: ReactionRule = null
    simRRMap.foreach(tm => {
      if (!GlobalCfg.checkData || tm._2.rule.check) {
        var nb: Bigraph = curBigraph.applyMatch(tm._2);
        GlobalCfg.SysClk = tm._1.split("_")(0).toInt
        /**
         * update a reaction rule data model
         */
        tm._2.rule.update
        /**
         * update agent data with clock
         */
        Data.updateDataCalcsWithClk(tm._2.rule.getRRIncr.toString)

        if (nb.root == null)
          nb.root = new Nil();
        curBigraph = nb
        curRR = tm._2.rule
      }
    })

    if (curBigraph != null && curRR != null) {
      var nv = new Vertex(curBigraph, v, curRR)
      nv.sysClk = GlobalCfg.SysClk
      if (g.lut.contains(nv.hash)) {
        nv = g.lut(nv.hash);
        nv.addParents(v)
      } else {
        g.add(nv);
      }
      v.addTarget(nv, curRR);
      forwardPath.enqueue(nv)
      if (GlobalCfg.printMode) {
        printf("%s:%s\n", "N_" + Math.abs(nv.hash), nv.bigraph.root.toString);
        println(nv.variables)
      }
    }
    matches.clear();
    DiscreteEventSimulator.matchGC;
    true;
  }

  def dumpDotForward(dot: String): String = ""

}
