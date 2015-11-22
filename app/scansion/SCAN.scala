package scansion

object SCAN {
    def clean(s:String):String = {
      s.replaceAll(" ","\\\\;")
    }

  sealed trait Syllable {
    val length:Boolean
    val left:String
    val space:String
    val vowel:String
    val right:String
  }

  sealed trait Foot {
    val syllables:List[Syllable]
    override def toString = syllables.map(_.toString).mkString("")
  }

  sealed trait FullFoot extends Foot {
    val syllables:List[Syllable]
  }

  sealed trait HalfFoot extends Foot {
    val syllables:List[Syllable]
  }
  
  sealed trait Line {
    val feet:List[Foot]

    override def toString = "|\\," + feet.map(_.toString).mkString("\\, | \\,") + "\\,|"
  }

  case class Long(l:String, s:String, v:String, r:String, 
    byPos:Boolean = true) extends Syllable {

    val length:Boolean = true

    val left:String = l
    val space:String = s
    val vowel:String = v
    val right:String = r

    val byPosition:Boolean = byPos


    def this(s:Short){
      this(s.left,s.space,s.vowel,s.right,false)
    }
    override def toString = {
      if (space.nonEmpty)
        s"${clean(left)} \\underset{\\smile}{ ${clean(space)} } \\overline{${clean(vowel)}} ${clean(right)}"
      else
        s"${clean(left)} \\overline{${clean(vowel)}} ${clean(right)}"
    }
  }

  case class Short(l:String, s:String, v:String, r:String) extends Syllable {
    val length = false

    val left:String = l
    val space:String = s
    val vowel:String = v
    val right:String = r

    override def toString = {
      if (space.nonEmpty)
        s"${clean(left)} \\underset{\\smile}{ ${clean(space)} } \\overset{\\smallsmile}{${clean(vowel)}} ${clean(right)}"
      else
        s"${clean(left)} \\overset{\\smallsmile}{${clean(vowel)}} ${clean(right)}"
    }
  }

  case class Dactyl(l:Long,s1:Short,s2:Short) extends FullFoot{
    val syllables = List(l,s1,s2)
  }

  case class Trochee(l:Long,s:Short) extends HalfFoot{
    val syllables = List(l,s)
  }

  case class Spondee(l1:Long,l2:Long) extends FullFoot with HalfFoot{
    val syllables = List(l1,l2)
  }
  
  case class DactylicHexameter(f1:FullFoot,f2:FullFoot,f3:FullFoot,f4:FullFoot,f5:Dactyl,f6:HalfFoot) extends Line{
    val feet = List(f1,f2,f3,f4,f5,f6)
  }
  case class EmptyLine() extends Line{
    val feet = List()
    override def toString = ""
  }
  case class BadLine() extends Line{
    val feet = List()
    override def toString = "Lexing Error"
  }
}
