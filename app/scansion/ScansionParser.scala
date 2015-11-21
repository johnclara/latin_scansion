package scansion

import com.codecommit.gll._
import SCAN._


object ScansionParser extends common.Example[Line] with RegexParsers {
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


  lazy val short: Parser[Short] = (
    "\\(S[\\w ]*\\)".r       ^^ { str => Short(str.substring(2,str.size-1)) }
  )

  lazy val long: Parser[Long] = (
    "\\(L[\\w ]*\\)".r       ^^ { str => Long(str.substring(2,str.size-1)) }
  )

  def parser = line
  
  def handleSuccesses(i:Int,forest: Stream[Line]) {
    forest.foreach( t =>
      println(s"$i : $t")
    )
  }
  
  def parseLine(l:String):List[String] = {
    val clean = l.toLowerCase.replaceAll("[^A-Za-z ]","")
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