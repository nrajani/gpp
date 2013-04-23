package gpp.convert

import scala.xml._

object Text2XML {

  //def toXml(lst:List[String]) = <item label={ lst(0) } tweetid={ lst(1) }><content>{ lst(5) }</content></item>;
  def main(args: Array[String]) {

  	val input = io.Source.fromFile(args(0)).getLines.map(x => x.split(";;").toList)

  	val xml_in = input.map{x => 
  		if(x(0)=="0")
  		x.updated(0,"negative")
  		else if(x(0)=="2")
  		x.updated(0,"neutral")
  		else
  		x.updated(0,"positive")}
  	
  	val xml = <root>{ xml_in.map(p => <item label={ p(0) } tweetid={ p(1) } date={ p(2) } target={ p(3) } username={ p(4) }><content>{ p(5) }</content></item>) }</root>
  	//xml.foreach(println)
  val pp = new scala.xml.PrettyPrinter(80, 2)
  println(pp.format(xml))
 }


}

    