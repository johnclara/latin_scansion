package common

import scala.io.Source
import com.codecommit.gll._
trait Example[A] extends Parsers {
  
  def main(args: Array[String]) {
    for (file <- args) {
      println(file)
      println("=============================")
      
      var lexingErrors = 0
      
      val results = (Source fromFile file).getLines.map( s => {

        import util.parsing.combinator.Parsers

        val clean = s.toLowerCase.replaceAll("[^A-Za-z ]","")
          .replaceAll(" ia|^ia"," ja")
          .replaceAll(" iu|^iu"," ju")
          .replaceAll(" io|^io"," jo")
          .replaceAll("iec","jec")
        (scansion.ScansionLexer.parseString(clean) match {
          case scansion.ScansionLexer.Success(lup,_) => lup.toString
          case _              => {
            lexingErrors += 1
            "error"
          }
        }).toList
      }).map( x =>
        parser(LineStream(x.toIterator))
      )
      
      var i = 1
      var parsingErrors = 0
      results.foreach( result =>{
        if (result exists { _.isInstanceOf[Success[A]] }) {
          handleSuccesses(i, for (Success(tree, _) <- result) yield tree)
        } else {
          val sorted = result.toList sortWith { _.tail.length < _.tail.length }
          val length = sorted.head.tail.length
          parsingErrors += 1
          
          for (Failure(msg, tail) <- sorted takeWhile { _.tail.length == length }) {
            val pattern = s"${i} : error:%%d: %s%n    %%s%n    %%s%n".format(msg)
            tail.printError(pattern)(System.err)
          }
        }
        i += 1
      }
      )
      
      println(s"$lexingErrors lexing errors and $parsingErrors parsing errors out of $i lines")
    }
  }

  def parser: Parser[A]
  
  def handleSuccesses(i:Int, forest: Stream[A])
}
