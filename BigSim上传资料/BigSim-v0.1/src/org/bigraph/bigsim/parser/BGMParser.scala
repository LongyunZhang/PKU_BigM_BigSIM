package org.bigraph.bigsim.parser

import java.io.File
import scala.util.parsing.combinator.RegexParsers
import org.bigraph.bigsim.data.DataModel
import org.bigraph.bigsim.model._
import org.bigraph.bigsim.data.Data

abstract class BGMTerm {
}

case class BGMControl(n: String, arity: Int, active: Boolean) extends BGMTerm
case class BGMNode(n: String, active: Boolean, ctrl: BGMControl) extends BGMTerm
case class BGMName(n: String, isouter: Boolean) extends BGMTerm
case class BGMRule(n: String, redex: String, reactum: String, exp: String) extends BGMTerm
case class BGMAgent(n: String) extends BGMTerm
case class BGMProp(n: String, p: String) extends BGMTerm
case class BGMNil() extends BGMTerm

object BGMTerm {
  def toBigraph(t: List[BGMTerm]): Bigraph = {
    val b: Bigraph = new Bigraph(1);
    // BGMControl
    val controlList: List[BGMControl] = t.filter(_ match {
      case BGMControl(_, _, _) => true
      case _ => false
    }).map(_.asInstanceOf[BGMControl])
    controlList.map(x => Bigraph.addControl(x.n, x.arity, x.active));

    // BGMName
    t.filter(_ match {
      case BGMName(_, _) => true
      case _ => false
    }).map(x => {
      val xo: BGMName = x.asInstanceOf[BGMName];
      if (xo.isouter) {
        b.addOuterName(new Name(xo.n, "outername"));
      } else {
        b.addInnerName(new Name(xo.n, "innername"));
      }

    });

    // 只读取一个model
    val agentList = t.filter(_ match {
      case BGMAgent(_) => true
      case _ => false
    }).head.asInstanceOf[BGMAgent];

    b.root = TermParser.apply(agentList.n);
    //println(b.root)

    // 目前认为Model中所有的name都不是free name
    //println("Model not free names:" + b.root.getAllNames);
    Bigraph.modelNames = b.root.getAllNames;

    // BGMRule
    t.filter(_ match {
      case BGMRule(_, _, _, _) => true
      case _ => false
    }).map(rr => {
      val rrp = rr.asInstanceOf[BGMRule]
      val redex = TermParser.apply(rrp.redex);
      val reactum = TermParser.apply(rrp.reactum);
      b.rules.add(new ReactionRule(rrp.n, redex, reactum, rrp.exp));
    });

    // 目前bigMC中没有声明的name全默认为inner name,只有%outer 说明的才是outer name，这里作一次修正
    Bigraph.nameMap.values.toList.map(x => b.inner.add(x));
    b.inner = b.inner.diff(b.outer);

    //BGMProp
    t.filter(_ match {
      case BGMProp(_, _) => true
      case _ => false
    }).map(p => {
      val pro = p.asInstanceOf[BGMProp];
      //MC.addProperty(pro.n, QueryParser.parse(pro.p))
    })
    b;
  }
}

object BGMParser extends RegexParsers {
  def exp = "[^;^{^}]*".r
  def ident = "[^ \t\n\r;]+".r
  def ws = "[ \t]*".r
  // 单行注释内容，解析时忽略
  def comment: Parser[String] = ".*".r;

  def EOL: Parser[String] = ws ~ ";" ~ ws ^^^ ""

  def stmt: Parser[BGMTerm] = "%active" ~> (ws ~> ident ~ (ws ~> ":" ~> ws ~> ident)) ^^ {
    case i ~ a => BGMControl(i, a.toInt, true)
  } |
    "%passive" ~> (ws ~> ident ~ (ws ~> ":" ~> ws ~> ident)) ^^ {
      case i ~ a => BGMControl(i, a.toInt, false)
    } |
    "%outername" ~> (ws ~> ident) ^^ { x => BGMName(x, true) } |
    "%outer" ~> (ws ~> ident) ^^ { x => BGMName(x, true) } |
    "%innername" ~> (ws ~> ident) ^^ { x => BGMName(x, false) } |
    "%inner" ~> (ws ~> ident) ^^ { x => BGMName(x, false) } |
    "%name" ~> (ws ~> ident) ^^ { x => BGMName(x, false) } |
    "%rule" ~> (ws ~> ident ~ (ws ~> exp) ~ ("{" ~> exp <~ "}")) ^^ {
      case i ~ s ~ k => {
        val rr = s.split("->")
        BGMRule(i, rr(0), rr(1), k)
      }
    } |
    "%rule" ~> (ws ~> ident ~ (ws ~> exp)) ^^ {
      case i ~ s => {
        val rr = s.split("->")
        BGMRule(i, rr(0), rr(1), "")
      }
    } |
    "%agent" ~> ((ws ~> exp) ~ ("{" ~> exp <~ "}")) ^^ {
      case i ~ e => {
        Data.parseAgentExpr(e)
        BGMAgent(i)
      }
    } |
    "%agent" ~> (ws ~> exp) ^^ { x => BGMAgent(x) } |
    "%property" ~> (ws ~> ident ~ (ws ~> exp)) ^^ { case i ~ p => BGMProp(i, p) } |
    "%check" ^^^ { BGMNil() } |
    "#" ~ comment ^^^ { BGMNil() }

  def stmtList: Parser[List[BGMTerm]] = stmt ~ (EOL ~> stmtList) ^^ { case x ~ xs => x :: xs } |
    stmt <~ EOL ^^ { x => x :: Nil } |
    stmt ~> stmtList ^^ { x => x }

  def parse(s: File): List[BGMTerm] = parseAll(stmtList, io.Source.fromFile(s).mkString) match {
    case Success(res, _) => res
    case e => throw new Exception(e.toString)
  }

  def parseFromString(str: String): List[BGMTerm] = parseAll(stmtList, str) match {
    case Success(res, _) => res
    case e => throw new Exception(e.toString)
  }
}

object testBGMParser {
  // for test
  def main(args: Array[String]) {
    println("My BGMParser!");
    val fileName: String = "MobileCloud/models/hotel.bgm";
    val p: List[BGMTerm] = BGMParser.parse(new File(fileName));
    println("p:" + p);
    var b = BGMTerm.toBigraph(p);
    println("Bigraph:" + b);
    println("getAllNames:" + b.root.getAllNames);

    b.rules.map(r => {
      println(r.name);
      println(r.reactum.getAllNames);
      println(r.redex.getAllNames);
      println();
    });
  }
}


