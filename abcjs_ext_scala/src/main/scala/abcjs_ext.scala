package Music

import scala.scalajs.js.annotation._
import scala.scalajs.js
import Note.{letterNames, letterNamesLooped}
import js.JSConverters._

@JSExportTopLevel("util")
object AbcJsExt {
  //val pianoNotes = Vector("C", "D", "E", "F", "g", "a", "b")
  //val halfSteps = Vector(1, 2, 2, 1, 2, 2, 2)

  @JSExport
  def getKey(header:String):String = {
    "K:.*".r.findFirstIn(header) match {
      case Some(x) => x
      case _ => "C"
    }
  }

  def isAccidental(x:String) = List("^","_","^^","__").contains(x)
  def isOctave(x:String) = List(",","'").contains(x)
  val abcRgx = "[=_^]*[ABCDEFGabcdefg][',]*".r
  val matchComment = """(?m)^((\w:)|%).*""".r
  //val matchCommand = """(?m)^(?![\w|%]:).*""".r

  def toNote(abcNote:String):Note = {
    val letter = "[ABCDEFGabcdefg]".r.findFirstIn(abcNote) match {
      case Some(x) => x
      case _ => ""
    }
    val octave = "[',]+".r.findFirstIn(abcNote) match {
      case Some(x) => 4 - x.count(_ == ',') + x.count(_ == ''')
      case _ => 4
    }
    val accidental = "[=_^]+".r.findFirstIn(abcNote) match {
      case Some(x) => x.replace("^", "#").replace("_","b").replace("=","n")
      case _ => ""
    }

    Note(letter, octave, accidental)
  }

  @JSExport
  def transpose(music:String, key:String):String = {
    /* TODO: 
     * - don't duplicate accidentals in key signature
     */
    val m = parse(music)
    val oldHeader = m("header")
    val oldKeyHeader = getKey(oldHeader)
    val oldKey = oldKeyHeader.split(":").map(_.trim).last

    //var newMusic = abcRgx.replaceAllIn(oldText, n => {
    //  toNote(n.toString).transposeWithKey(oldKey, key, usingKeySig=true).toAbc
    //})

    val xs = music.split("\\n")
    val numLines = xs.size
    var newMusic = Vector.tabulate(numLines){ lineNum => 
      xs(lineNum).matches(matchComment.toString) match {
        case true => xs(lineNum)
        case _ => abcRgx.replaceAllIn(xs(lineNum), n => {
          toNote(n.toString).transposeWithKey(oldKey, key, usingKeySig=true).toAbc
        })
      }
    }.mkString("\n")

    newMusic = newMusic.replace(oldKeyHeader, s"K:$key\n")
    println(newMusic)

    sanitize(newMusic)
  }


  @JSExport
  def sanitize(music:String):String = {
    music.split("\n").map(_.trim).filterNot(_ == "").mkString("\n")
  }

  def isHeader(x:String):Boolean = {
    // headers start with % or a character then a :
    x.head == '%' || "^\\w+:".r.findFirstIn(x).size > 0
  }

  @JSExport
  def parse(music:String) = {
    lazy val musicList = sanitize(music).split("\n")
    val (header, text) = musicList.partition(isHeader)
    js.Dictionary(
      "header" -> {header.mkString("\n") + "\n"},
      "text" -> {text.mkString("\n") + "\n"}
    )
  }

  @JSExport
  val keySigs = Note.keySigs.keys.toJSArray

  @JSExport
  def tester() {
    println("Printed Tests:")
    println("sharpOrder: " + Note.sharpOrder.toString)
    println("flatOrder: "  + Note.flatOrder)
    println("Circle 5: "   + Note.circleOf5th)
    println("Circle 4: "   + Note.circleOf4th)
    println("KeySigs: " + Note.keySigs.toList)
  }
}

/* I want to ignore lines that start with %% and _:___
val x = """L:1/1
K:C
V:1
[FA]|[FA]|[EG]||
V:2 clef=bass
[D,C]|[G,B,]|[C,B,]||

CDEFGA_B=BC'"""

val matchComment = """(?m)^[\w|%]:.*""".r
val matchCommand = """(?m)^(?![\w|%]:).*""".r

val xs = x.split("\\n")
val numLines = x.split("\\n").size
Vector.tabulate(numLines){ lineNum => 
  xs(i).matches(matchComment.toString) match {
    case true => xs(i)
    case _ => abcRgx.replaceAllIn(xs(i), n => {
      toNote(n.toString).transposeWithKey(oldKey, key, usingKeySig=true).toAbc
    })
  }
}
 */
