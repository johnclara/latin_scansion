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
  val elision = "(" + consonants + "(?:" + vowel + "|" + diphthong + ")" + "m?)( +h?)"

  val elidedVowelDigraphR = (
    elision +
    "(" + vowel + ")" +
    "(" + spaces + "(?:(?:" + mute + liquid + ")|" + digraph + ")" + spaces + ")"
  ).r
  val vowelDigraphR = (
    "(" + consonants + ")" +
    "(" + vowel + ")" +
    "(" + spaces + "(?:" + mute + liquid + "|" + digraph + ")" + spaces + ")"
  ).r

  val elidedDoubleConsonantR =  (
    elision + 
    "(" + vowel + ")" + 
    "(" + spaces + doubleConsonant + spaces + consonants + spaces + ")"
  ).r

  val doubleConsonantR =  (
    "(" + consonants + ")" +
    "(" + vowel + ")" +
    "(" + spaces + doubleConsonant + spaces + consonants + spaces + ")"
  ).r

  val elidedDiphthongR =  (
    elision + 
    "(" + diphthong + ")" +
    "(" + spaces + consonants + spaces + ")"
  ).r

  val diphthongR =  (
    "(" + consonants + ")" + 
    "(" + diphthong + ")" + 
    "(" + spaces + consonants + spaces + ")"
  ).r

  val elidedSingleConsonantR = (
    elision + 
    "(" + vowel + ")" +
    "(" + spaces + singleConsonant+"?" + spaces + ")"
  ).r
  val singleConsonantR = (
    "(" + consonants + ")" +
    "(" + vowel + ")" + 
    "(" + spaces + singleConsonant+"?" + spaces + ")"
  ).r

  def line:Parser[lin] = (
    syllable~line   ^^ { case s~l => Cons(l,s) }
  | syllable        ^^ { case s => s}
  | "^$".r          ^^ { case s => Empty()})


  def syllable: Parser[syll] = (
    elidedVowelDigraphR ^^ { str => str match {
      case elidedVowelDigraphR(l,s,v,rv) => syll(s"0${l}1${s}2${v}3${rv}4")
      }
    }
  | vowelDigraphR ^^ { str => str match {
      case vowelDigraphR(l,v,rv) => syll(s"0${l}12${v}3${rv}4")
      }
    }
  | elidedDoubleConsonantR ^^ { str => str match {
      case elidedDoubleConsonantR(l,s,v,rv) => syll(s"9${l}1${s}2${v}3${rv}4")
      }
    }
  | doubleConsonantR ^^ { str => str match {
      case doubleConsonantR(l,v,rv) => syll(s"9${l}12${v}3${rv}4")
      }
    }
  | elidedDiphthongR ^^ { str => str match {
      case elidedDiphthongR(l,s,v,rv) => syll(s"9${l}1${s}2${v}3${rv}4")
      }
    }
  | diphthongR ^^ { str => str match {
      case diphthongR(l,v,rv) => syll(s"9${l}12${v}3${rv}4")
      }
    }
  | elidedSingleConsonantR ^^ { str => str match {
      case elidedSingleConsonantR(l,s,v,rv) => syll(s"0${l}1${s}2${v}3${rv}4")
      }
    }
  | singleConsonantR ^^ { str => str match {
      case singleConsonantR(l,v,rv) => syll(s"0${l}12${v}3${rv}4")
      }
    }
  )
}

object ScansionLexer extends ScansionLexer {
  def parseString(s:String):ParseResult[scansion.lin] = parseAll(line,s)
}
