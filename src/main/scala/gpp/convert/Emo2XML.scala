package gpp.convert

import scala.xml._
import java.io.{File,BufferedReader,FileReader}

object Emo2XML {

  //def toXml(lst:List[String]) = <item label={ lst(0) } tweetid={ lst(1) }><content>{ lst(5) }</content></item>;
  def main(args: Array[String]) {
    val topDir = new File(args(0))
    val raw_input = topDir.listFiles.toIterator.filter(x => x.getName.endsWith(".txt")).toList
   // println(raw_input(0).getName)
    var combi_xml=List[List[String]]()
    for(i <- 0 to raw_input.length-1){
  	val input = io.Source.fromFile(raw_input(i)).getLines.map(x => x.split("\t").toList)
    //if(raw_input(i).getName.startsWith("happy"))
    val xml_in = input.map{x => 
      if(raw_input(i).getName=="sad.txt")
      x.updated(0,"negative")
      else if(raw_input(i).getName=="neutral.txt")
      x.updated(0,"neutral")
      else
      x.updated(0,"positive")}.toList
      combi_xml = combi_xml ++ xml_in
    }
    val xml = <root>{ combi_xml.map(p => <item label={ p(0) } tweetid={ p(1) } target={ "unknown" }><content>{ p(2) }</content></item>) }</root>
    //xml.foreach(println)
  val pp = new scala.xml.PrettyPrinter(80, 2)
  println(pp.format(xml)) 

}

}
