package scansion

import util.parsing.combinator._
import util.Try

abstract trait lin
case class Empty() extends lin {
  override def toString = {
    ""
  }
}
case class Cons(l:lin,s:syll) extends lin {
  override def toString = {
    s.toString + l.toString
  }
}
case class syll(word:String) extends lin {
  override def toString = {
    word
  }
}

class ScansionLexer extends RegexParsers {
  val vowel = "[aeiouy]"
  val mute = "[bcdfgpt]"
  val liquid = "[lr]"
  val digraph = "(?:ch|ph|th|qu)"
  val singleConsonant = "(?:" + digraph + "|[^aeiouyhx\\W_0-9])"
  val diphthong = "(?:oi|ae|oe|au|ei|cui|hui)"
  val spaces = "(?: *)"
  val doubleConsonant = "(?:(?:" + singleConsonant + spaces + "(?:" + singleConsonant + "|x))|x)"
  val consonants = "(?:(?:" + singleConsonant + "|[xh])*)"


  def line:Parser[lin] = (
    syllable~line   ^^ { case s~l => Cons(l,s) }
  | syllable        ^^ { case s => s}
  | "^$".r          ^^ { case s => Empty()})


  def syllable: Parser[syll] = (
    ("(?:" + consonants + "(?:" + vowel + "|" + diphthong + ")" + "(?:m? +h?)|" + consonants + ")" + vowel + spaces + "(?:(?:" + mute + liquid + ")|" + digraph + ")" + spaces).r ^^ { str => syll("(S" + str + ")") }
  | ("(?:" + consonants + "(?:" + vowel + "|" + diphthong + ")" + "(?:m? +h?)|" + consonants + ")" + vowel + spaces + doubleConsonant + spaces + consonants + spaces).r           ^^ { str => syll("(L" + str + ")") }
  | ("(?:" + consonants + "(?:" + vowel + "|" + diphthong + ")" + "(?:m? +h?)|" + consonants + ")" + diphthong + spaces + consonants + spaces).r                                  ^^ { str => syll("(L" + str + ")") }
  | ("(?:" + consonants + "(?:" + vowel + "|" + diphthong + ")" + "(?:m? +h?)|" + consonants + ")" + vowel + spaces + singleConsonant+"?" + spaces).r                             ^^ { str => syll("(S" + str + ")") }
  )
}

object ScansionLexer extends ScansionLexer {
  def parseString(s:String):ParseResult[scansion.lin] = parseAll(line,s)
}
