package gpp.classify

import nak.NakContext._
import nak.core._
import nak.data._
import nak.liblinear.{LiblinearConfig,SolverType}
import nak.util.{GrowableIndex, ConfusionMatrix}
import java.io._
import scala.xml._
import org.apache.log4j.Level
import org.apache.log4j.Logger
//import gpp.classify.Tweet

object Classify {

  def main(args: Array[String]) {

   val opts = ClassifyOpts(args)

  val logLevel = if (opts.verbose()) Level.DEBUG else Level.INFO
    Logger.getRootLogger.setLevel(logLevel)
  if(opts.method() == "majority")
  Classification.maxClass(opts.trainfile(),opts.evalfile(),opts.detail())

  if(opts.method() == "lexicon")
  Classification.maxToken(opts.evalfile(),opts.detail())

  if(opts.method() == "L2R_LR"){
    if(opts.ext())
    Classification.libLinearExt(opts.trainfile(),opts.evalfile(),opts.cost(),opts.detail())
    else
    Classification.libLinear(opts.trainfile(),opts.evalfile(),opts.cost(),opts.detail())  
  }
  

    }
}


/**
 * An object that sets up the configuration for command-line options using
 * Scallop and returns the options, ready for use.
 *
 */
object ClassifyOpts {

  import org.rogach.scallop._
  
  def apply(args: Array[String]) = new ScallopConf(args) {
    banner("""
  Classification application.

  For usage see below 
       """)
    val methods = Set("lexicon","majority","L2R_LR")

    val trainfile = opt[List[String]]("train", short = 't',required=false,descr="The file containing training events.")

    val evalfile = opt[List[String]]("eval", short='e', descr="The file containing evalualation events.")

    val method = opt[String]("method", short='m', default=Some("L2R_LR"), validate = methods, descr = "The type of solver to use. Possible values: majority, lexicon, or any liblinear solver type.")

    val cost = opt[Double]("cost", short='c',default=Some(1.0), required=false, descr="The cost parameter C. Bigger values means less regularization (more fidelity to the training set).")

    val ext = opt[Boolean]("extended", short='x', required=false, descr="Use extended features.")

    val verbose = opt[Boolean]("verbose",short='v')

    val detail = opt[Boolean]("detailed", short='d', required=false)
  }
}
