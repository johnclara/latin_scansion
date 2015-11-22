package scansion

import com.codecommit.gll._
import SCAN._


object ScansionParser extends RegexParsers {
  lazy val line: Parser[Line] = (
    foot ~ foot ~ foot ~ foot ~ dactyl ~ halfFoot ^^ { (e1,e2,e3,e4,e5,e6) => DactylicHexameter(e1,e2,e3,e4,e5,e6) }
  | "^$".r                                        ^^ { (e1)                => EmptyLine() }
  | "^error$".r                                   ^^ { (e1)                => BadLine() }
  )

  lazy val dactyl: Parser[Dactyl] = (
    long  ~ short ~ short ^^   { (e1,e2,e3) => Dactyl(e1,e2,e3) }
  | short ~ short ~ short ^^   { (e1,e2,e3) => Dactyl(new Long(e1),e2,e3) }
  )

  lazy val foot: Parser[FullFoot] = (
    long  ~ short ~ short ^^   { (e1,e2,e3) => Dactyl(e1,e2,e3) }
  | short ~ short ~ short ^^   { (e1,e2,e3) => Dactyl(new Long(e1),e2,e3) }
  | long  ~ long          ^^   { (e1,e2)    => Spondee(e1,e2) }
  | short ~ long          ^^   { (e1,e2)    => Spondee(new Long(e1),e2) }
  | long  ~ short         ^^   { (e1,e2)    => Spondee(e1,new Long(e2)) }
  | short ~ short         ^^   { (e1,e2)    => Spondee(new Long(e1),new Long(e2)) }
  )

  lazy val halfFoot: Parser[HalfFoot] = (
    long  ~ long ~ "\\n|$".r       ^^    { (e1,e2,_) => Spondee(e1,e2) }
  | short ~ long ~ "\\n|$".r       ^^    { (e1,e2,_) => Spondee(new Long(e1),e2) }
  | long  ~ short ~ "\\n|$".r      ^^    { (e1,e2,_) => Trochee(e1,e2) }
  | short ~ short ~ "\\n|$".r      ^^    { (e1,e2,_) => Trochee(new Long(e1),e2) }
  )


  val word = "([a-z ]*)"
  val base = s"${word}1${word}2${word}3${word}4"
  val shortR = ("0" + base).r
  val longR = ("9" + base).r

  lazy val short: Parser[Short] = (
    shortR  ^^ { str => str match {
      case shortR(l,s,v,r) => Short(l,s,v,r) 
      }
    }
  )

  lazy val long: Parser[Long] = (
    longR  ^^ { str => str match {
      case longR(l,s,v,r) => Long(l,s,v,r) 
      }
    }
  )

  def parser = line
  
  def handleSuccesses(i:Int,forest: Stream[Line]) {
    forest.foreach( t =>
      println(s"$i : $t")
    )
  }
  
  def parseLine(l:String):List[String] = {
    val clean = l.toLowerCase.replaceAll("[^A-Za-z ]","")
      .replaceAll(" +"," ")
      .replaceAll(" *$","")
      .replaceAll(" ia|^ia"," ja")
      .replaceAll(" iu|^iu"," ju")
      .replaceAll(" io|^io"," jo")
      .replaceAll("iec","jec")
    val lexedString = {   
      import util.parsing.combinator.Parsers

     (scansion.ScansionLexer.parseString(clean) match {
        case scansion.ScansionLexer.Success(lup,_) => lup.toString
        case _              => "error"
     }).toList.toIterator
    }
    val result = parser(LineStream(lexedString))
    if (result exists { _.isInstanceOf[Success[Line]] }) {
      result.map{ case Success(tree, _) => tree.toString}.toList
    } else {
      List()
    }
  }
}
