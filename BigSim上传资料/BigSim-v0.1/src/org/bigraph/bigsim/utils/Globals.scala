package org.bigraph.bigsim.utils

import java.util.Properties
import java.io.FileInputStream

object GlobalCfg {

  val prop: Properties = new Properties

  try {
    prop.load(new FileInputStream("config.properties"))
  } catch {
    case e: Exception =>
      e.printStackTrace
      sys.exit(1)
  }

  /**
   * For condition configurations
   */
  val condPrefStr: String = prop.getProperty("condPrefStr")
  val exprPrefStr: String = prop.getProperty("exprPrefStr")
  val sysClkPrefStr: String = prop.getProperty("sysClkPrefStr")
  val randomPrefStr: String = prop.getProperty("randomPrefStr")
  val wExprPrefStr: String = prop.getProperty("wExprPrefStr")
  val hmmPrefStr: String = prop.getProperty("hmmPrefStr")
  val ratePrefStr: String = prop.getProperty("ratePrefStr")
  val reversePrefStr: String = prop.getProperty("reversePrefStr")
  val reactPrefStr: String = prop.getProperty("reactPrefStr")
  val minProbability: Double = prop.getProperty("minProbability").toDouble

  val SimulatorClass: String = prop.getProperty("SimulatorClass")

  var DEBUG: Boolean = prop.getProperty("DEBUG").toBoolean
  var checkLocal: Boolean = prop.getProperty("checkLocal").toBoolean
  var maxSteps: Long = prop.getProperty("maxSteps").toLong
  var reportInterval: Long = prop.getProperty("reportInterval").toLong
  var printMode: Boolean = prop.getProperty("printMode").toBoolean
  var ranNameIndex: Int = prop.getProperty("ranNameIndex").toInt

  /**
   *  set inputs: models, data, hmm
   */
  var fileSeparator: String = prop.getProperty("fileSeparator")
  var inputPath: String = prop.getProperty("inputPath")
  var modelName: String = prop.getProperty("modelName")
  var filename: String = inputPath + fileSeparator + "models" + fileSeparator + modelName + ".bgm"
  // whether check data
  var checkData: Boolean = prop.getProperty("checkData").toBoolean
  var dataInput: String = {
    if (checkData) inputPath + fileSeparator + "data" + fileSeparator + modelName + ".data"
    else ""
  }
  // whether check HMM
  var checkHMM: Boolean = prop.getProperty("checkHMM").toBoolean
  var hmmInput: String = {
    if (checkHMM) inputPath + fileSeparator + "hmms" + fileSeparator + modelName + ".hmm"
    else ""
  }
  // whether check sorting
  var checkSorting: Boolean = prop.getProperty("checkSorting").toBoolean
  var sortingInput: String = {
    if (checkSorting) inputPath + fileSeparator + "sortings" + fileSeparator + modelName + ".xml"
    else ""
  }
  // whether check interest pattern
  var checkInterestPattern: Boolean = prop.getProperty("checkInterestPattern").toBoolean
  var interestPatternInput: String = {
    if (checkInterestPattern) inputPath + fileSeparator + "patterns" + fileSeparator + modelName + ".xml"
    else ""
  }

  /**
   * set outputs: paths, results
   */
  var outputPath: Boolean = prop.getProperty("outputPath").toBoolean
  var pathOutput: String = {
    if (outputPath) inputPath + fileSeparator + "paths" + fileSeparator + modelName + ".path"
    else ""
  }
  var outputGraph: Boolean = prop.getProperty("outputGraph").toBoolean
  var graphOutput: String = {
    if (outputGraph) inputPath + fileSeparator + "results" + fileSeparator + modelName + ".dot"
    else ""
  }
  var outputData: Boolean = checkData && prop.getProperty("outputData").toBoolean
  var dataOutput: String = {
    if (outputData) inputPath + fileSeparator + "paths" + fileSeparator + modelName + ".data"
    else ""
  }

  // system clock init
  var SysClk: Double = prop.getProperty("initSysClk").toDouble
  // system clock increaser
  var SysClkIncr: Double = prop.getProperty("sysClkIncr").toDouble
  // the max system clock
  var maxSysClk: Double = prop.getProperty("maxSysClk").toDouble

  def getRanNameIndex: Int = {
    ranNameIndex += 1
    ranNameIndex
  }

  // how many times of simulation
  var simLoop: Int = prop.getProperty("simLoop").toInt
  var curLoop: Int = 0;
  var append: Boolean = false

  var allDefs: Boolean = false
  var allUses: Boolean = false

  var patternFile: String = ""
  var defPathMapFile: String = ""

  var node: Boolean = true
  var verbose: Boolean = false
  var printDiscovered: Boolean = false
  var localCheck: Boolean = false
  var reportFrequency: Int = 500
  var stochastic: Boolean = false
}
