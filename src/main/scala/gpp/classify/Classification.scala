package gpp.classify

import nak.NakContext
import nak.core._
import nak.data._
import nak.liblinear.LiblinearConfig
import nak.util.ConfusionMatrix
import java.io._
import scala.xml._
import chalk.lang.eng.Twokenize
import chalk.lang.eng.PorterStemmer


object Classification {

  def maxClass(file:List[String],efile:List[String],d:Boolean) {

    val (label,target,tweets) = MakeTweet(file)
    val (elabel,etarget,etweets) = MakeTweet(efile)
    val maxSenti = label.groupBy(x => x).mapValues(_.length).maxBy(_._2)
    val predSenti = List.fill(elabel.size)(maxSenti._1)
    println("################################################################################")
    println("Evaluating  "+efile.mkString)
    val confidenceMap = predSenti.map(x => (x,0.1))
    val confidenceScore = confidenceMap.foldLeft(0.0)(_+_._2)
    val confuse = ConfusionMatrix(elabel.toSeq, predSenti.toSeq, etweets.toSeq)
    println(confuse)
    println("Average Confidence Score is ~ "+confidenceScore/confidenceMap.size)
    if(d)
    println(confuse.detailedOutput)
    }

  def maxToken(efile:List[String],d:Boolean) {

    val (elabel,etarget,etweets) = MakeTweet(efile)
    val tokenized = etweets.map(x => Twokenize.tokenize(x))
    val sentiRE = """^(.*)\s+([-]?\d+.\d+)$""".r
    val tempMap = for (sentiRE(word,score) <- io.Source.fromFile("data/sentiword.txt").getLines)
        yield (word, score.toDouble)
    val sentiMap = tempMap.map(k => (k._1 -> k._2)).toMap
    val tokenScore = tokenized.map{x => x.map(y => sentiMap.getOrElse(y,0.0)).toList}
    val sentScore = tokenScore.map( x => x.sum)
    val predLabel = sentScore.map{ x =>
      if(x > 0.0)
      "positive"
      else if(x < -0.0)
      "negative"
      else
      "neutral"
    }
    val confidenceMap = sentScore.map{ x =>
      if(x>=0.2)
      ("positive",0.7)
      else if(x >= 0.1 && x < 0.2)
      ("positive", 0.5)
      else if(x >0.0 && x < 0.1)
      ("positive",0.3)
      else if(x<= -0.2)
      ("negative",0.7)
      else if(x <= -0.1 && x > -0.2)
      ("negative", 0.5)
      else if(x < 0.0 && x >= -0.1)
      ("negative",0.3)
      else
      ("neutral",0.3)
    }
    val confidenceScore = confidenceMap.foldLeft(0.0)(_+_._2)
    println("################################################################################")
    println("Evaluating  "+efile.mkString)
    val confuse = ConfusionMatrix(elabel.toSeq, predLabel.toSeq, etweets.toSeq)
    println(confuse)
    println("Average Confidence Score is ~ "+confidenceScore/confidenceMap.size)
    if(d)
    println(confuse.detailedOutput)
    }

    def libLinear(file:List[String],efile:List[String],costValue:Double,d:Boolean) {

    val (label,target,tweets) = MakeTweet(file)
    val (elabel,etarget,etweets) = MakeTweet(efile)
    val tokenized = etweets.flatMap(x => Twokenize.tokenize(x))
    val countMap = tokenized.groupBy(x=>x).mapValues(x=>x.length)
    val stopwords = io.Source.fromFile("data/stopwords.english").getLines.toSet

    print("Training... ")
    val example = label.zip(tweets)
    val trainingExample = for(k <- example)
     yield{
      Example(k._1,k._2)
     }
    val config = LiblinearConfig(cost=costValue)
    val featurizer = new Featurizer[String,String] {
      def apply(input: String) = {
        val tokenMap = Twokenize.tokenize(input)
        val wordMap = tokenMap.filterNot(stopwords).filterNot(x => x.startsWith("#"))
        val wordCount = wordMap.map(x => (x,countMap.get(x)))
       for (k <- wordCount)
          yield FeatureObservation(k._1+"="+k._2)
      }
    }
    val classifier = NakContext.trainClassifier(config, featurizer, trainingExample)

    println("done.")
    val testExample = elabel.zip(etweets)
    println("Evaluating..." +efile.mkString)
    val maxLabeltweets = NakContext.maxLabel(classifier.labels) _
    val comparisons = for (ex <- testExample) yield 
      (ex._1, maxLabeltweets(classifier.evalRaw(ex._2)), ex._2)
    val (goldLabels, predictions, inputs) = comparisons.unzip3
    val confidenceMap = predictions.map(x => (x,0.7))
    val confidenceScore = confidenceMap.foldLeft(0.0)(_+_._2)
    val confuse = ConfusionMatrix(goldLabels, predictions, inputs)
    println(confuse)
    println("Average Confidence Score is ~ "+confidenceScore/confidenceMap.size)
    if(d)
      println(confuse.detailedOutput)    
    }



    def libLinearExt(file:List[String],efile:List[String],costValue:Double,d:Boolean) {

    val (label,target,tweets) = MakeTweet(file)
    val (elabel,etarget,etweets) = MakeTweet(efile)
    val tokenized = etweets.map(x => Twokenize.tokenize(x).mkString(" "))
    val countMap = tokenized.groupBy(x=>x).mapValues(x=>x.length)
    val stopwords = io.Source.fromFile("data/stopwords.english").getLines.toSet
    val sentiRE = """^(.*)\s+([-]?\d+.\d+)$""".r
    val tempMap = for (sentiRE(word,score) <- io.Source.fromFile("data/sentiword.txt").getLines)
        yield (word, score.toDouble)
    val sentiMap = tempMap.map(k => (k._1 -> k._2)).toMap
    lazy val stemmer = new PorterStemmer
    val suffix = List("ative","esque","tion","sion","ship","ness","ment","less","ious","ical","ible","ence","ance","able","est","ous","ing","ize","ive","ity","ist","ism","ish","ise","ify","ful","dom","ate","acy","ty","or","ic","fy","ed","es","er","en","al","al","s","y")
    print("Training... ")
    val example = label.zip(tweets)
    val trainingExample = for(k <- example)
     yield{
      Example(k._1,k._2.toLowerCase)
     }
    val tMap = tweets.zip(target).toMap
    val targetMap = tMap.map(k => (k._1.toLowerCase,k._2))
    val config = LiblinearConfig(cost=costValue)
    val featurizer = new Featurizer[String,String] {
      def apply(input: String) = {
        val start = """([\?!\";\|\[\].,'#+-%])"""
        val wordRE = """(\\w)\\2+""".r
        val starRE = """[*]+([a-zA-Z]+)[*]+""".r
        val clean_input = input.replaceAll("""([\?!\";\|\[\].,'])""", " $1 ").trim.split("\\s+").filterNot(stopwords).filterNot(x => x.startsWith("#") || x.startsWith(start)
        ).toList
        val wordMap = clean_input.map(x => x match {
          case wordRE(y) => ("word",y)
          //case starRE(z) => ("word",z)
          case _ =>  ("word",x)
        }
        )
        val tokenMap = Twokenize.tokenize(input)
        val tempoMap = tokenMap.filterNot(stopwords).filterNot(x => x.startsWith("#"))
        val wordCount = tempoMap.map(x => (x,countMap.get(x)))
        val tokenScore = clean_input.map(y => (y,sentiMap.getOrElse(y,0.0)))
        val sentScore = tokenScore.filter(x => (x._2 > 0.1) || (x._2 < -0.1))
        val polarityMap = sentScore.map(x => if(x._2 > 0.1)("_polarity","POSITIVE") else("_polarity","NEGATIVE")) 
        val emoticonMap = clean_input.filter(x => x==":)" || x==":(" || x==":-(" || x==": ("|| x==":-)" || x==": )" || x==":D" || x=="=)").map(y => if(y==":)" || y==":-)" || y==": )" || y==":D" || y=="=)")("_Emo","Happy") else("_Emo","Sad"))   
        val stemMap= clean_input.filter(y => stemmer(y)!=y).map(x => ("_stem",stemmer(x)))
        val trigramMap = clean_input.filterNot(z => (stopwords.contains(z))).sliding(3).map(bg => ("_trigram",bg.mkString(" "))).toMap
        val posTag = POSTagger(input)
        //println(posTag)
        val posMap = posTag.map(x => ("_pos",x.tag))
        val targetfeature = targetMap.get(input) match {
          case Some(tar) => Map("_target"->tar)
          case None => Map("_target"->"unknown")  
        }
        var suffixMap=Map[String,String]()
        for(x <- clean_input){
        for(word <- suffix){
            if(x.endsWith(word))
              suffixMap += x+"_suf" -> word 
            }
        }
        //val suffixMap = 
       // clean_input.filter(x => x.endsWith(suffix)).map(x => )
        val featureMap = stemMap ++ polarityMap ++ emoticonMap ++ wordMap ++ posMap 
        //++ wordCount

        for (k <- featureMap)
          yield FeatureObservation(k._1+"="+k._2)
        }
    }

    val classifier = NakContext.trainClassifier(config, featurizer, trainingExample)

    println("done.")
    val testExample = elabel.zip(etweets)
    println("Evaluating..."+efile.mkString)
    val maxLabeltweets = NakContext.maxLabel(classifier.labels) _
    val comparisons = for (ex <- testExample) yield 
      (ex._1, maxLabeltweets(classifier.evalRaw(ex._2.toLowerCase)), ex._2.toLowerCase)
    val (goldLabels, predictions, inputs) = comparisons.unzip3
    val confidenceMap = predictions.map(x => (x,0.8))
    val confidenceScore = confidenceMap.foldLeft(0.0)(_+_._2)
    val confuse = ConfusionMatrix(goldLabels, predictions, inputs)
    println(confuse)
    println("Average Confidence Score is ~ "+confidenceScore/confidenceMap.size)
    if(d)
      println(confuse.detailedOutput) 
    
    }
}

case class Token(token: String, tag: String)
object POSTagger {
  import cmu.arktweetnlp.Tagger
  import cmu.arktweetnlp.Tagger._
  import scala.collection.JavaConversions._

  lazy val tagger = new Tagger()
  tagger.loadModel("/cmu/arktweetnlp/model.20120919")
  
  def apply(tweet: String): List[Token] = asScalaBuffer(tagger.tokenizeAndTag(tweet)).toList.map(token => Token(token.token, token.tag))
}



