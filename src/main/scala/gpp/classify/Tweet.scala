package gpp.classify
import scala.xml._
import java.io._

object MakeTweet
{
	def apply(file:List[String]):(List[String],List[String],List[String])={
	
    var targetList = List[String]()
    var sentiList = List[String]()
    var tweetList = List[String]()	
    for(num <- 0 to file.length-1){
    val rawExamples= XML.loadFile(file(num))
    var i =1
    while(i < rawExamples.child.length){
    val tt  = (rawExamples.child(i) \\ "item" \\ "content").text
    var sent = (rawExamples.child(i) \\ "item" \ "@label").text
    val target  = (rawExamples.child(i) \\ "item" \ "@target").text
    tweetList ::=  tt.toString
    sentiList ::=  sent.toString
    targetList ::= target.toString
  
  i = i+2
}

}

val combinedList = (tweetList,sentiList,targetList).zipped.toList
//println(combinedList)
val filteredList = combinedList.filter(x => x._2=="positive" || x._2=="negative" || x._2=="neutral")
val (xtweetList,xsentiList, xtargetList) = filteredList.unzip3
return (xsentiList, xtargetList, xtweetList)
	}
}
