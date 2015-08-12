package org.bigraph.bigsim.BRS

import scala.collection.mutable.Set
import org.bigraph.bigsim.data.DataModel
import org.bigraph.bigsim.model.Bigraph
import org.bigraph.bigsim.model.ReactionRule
import org.bigraph.bigsim.data.Data

/**
 * @author zhaoxin
 * version 0.1
 *
 * @author liangwei
 * version 0.2
 */

/**
 * Each vertex in the graph is an agent, and it related to a bigraph.
 */
class Vertex(b: Bigraph, v: Vertex, rr: ReactionRule) {
  var visited: Boolean = false
  var terminal: Boolean = false
  var parent: Vertex = v
  var pathVisited: Boolean = false

  var sysClk: Double = 0
  var reactionRule: ReactionRule = rr
  var reactionRules: Set[ReactionRule] = Set()
  var variables: String = Data.getValues(",")
  var bigraph: Bigraph = b
  var hash: Int = {
    if (bigraph.root != null)
      bigraph.root.toString.hashCode();
    else "".hashCode();
  }

  var parents: scala.collection.immutable.Set[Vertex] = scala.collection.immutable.Set(v)

  def this(b: Bigraph, v: Vertex, rrs: Set[ReactionRule], isSet: Boolean) = {
    this(b, v, null)
    reactionRules ++ rrs
  }

  def addParents(v: Vertex) {
    parents += v
  }

  // use Map instead of the 'set<pair<node *,reactionrule *> > target;' of C++ version.

  var target: Map[Vertex, ReactionRule] = Map();

  def addTarget(v: Vertex, rr: ReactionRule) {
    target += (v -> rr)
  }

  // use Map instead of the 'set<pair<node *,reactionrule *> > target;' of C++ version.
  /**
   * Here we use the set pair format for multiple reactions
   */
  var targets: Map[Vertex, Set[ReactionRule]] = Map();
  //var targets: Set[Tuple2[Vertex, Set[ReactionRule]]] = Set();

  def addTargets(rrs: Set[ReactionRule], v: Vertex) {
    targets += v -> rrs
    //targets += ((v, rrs))
  }
}

