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
  //val abcRgx = "[=_^]*[ABCDEFGabcdefg][',]*".r
  //val abcRgx = "(?!<\")[=_^]*[ABCDEFGabcdefg][',]*".r // js doesn't support this

  val breaker = ";;;;;"
  val chordRgx = "\"[ABCDEFGabcdefg][#b]?[^\"]*\"".r
  val shortChordRgx = "\"[=_^]*[ABCDEFGabcdefg]".r
  val abcRgx = "\"?[=_^]*[ABCDEFGabcdefg][',]*".r


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

    val xs = music.split("\\n")
    val numLines = xs.size
    var newMusic = Vector.tabulate(numLines){ lineNum => 
      xs(lineNum).matches(matchComment.toString) match {
        case true => xs(lineNum)
        //case _ => abcRgx.replaceAllIn(xs(lineNum), n => {
        //  toNote(n.toString).transposeWithKey(oldKey, key, usingKeySig=true).toAbc
        //})
        case _ => {
          val y = chordRgx.replaceAllIn(xs(lineNum), n => {
            val tmp = n.toString + breaker
            tmp(2) match {
              case '#' => "\"^" + tmp(1) + tmp.drop(3)
              case 'b' => "\"_" + tmp(1) + tmp.drop(3)
              case _ => tmp
            }
          })

          abcRgx.replaceAllIn(y, n => n.toString match {
            // replace chord
            case z if z.matches(shortChordRgx.toString) => 
              "\"" + toNote(z.toString.tail).transposeWithKey(oldKey, key, usingKeySig=false).toString.replaceAll("\\d","")
            // replace note
            case _ => toNote(n.toString).transposeWithKey(oldKey, key, usingKeySig=true).toAbc
          }).replace(breaker,"")
        }

      }
    }.mkString("\n")

    newMusic = newMusic.replace(oldKeyHeader, s"K:$key\n")

    sanitize(newMusic)
  }

  def transposeOctave(music:String, up:Boolean):String = {
    val xs = music.split("\\n")
    val numLines = xs.size

    val newMusic = Vector.tabulate(numLines){ lineNum => 
      xs(lineNum).matches(matchComment.toString) match {
        case true => xs(lineNum)
        case _ => {
          val y = chordRgx.replaceAllIn(xs(lineNum), n => {
            val tmp = n.toString + breaker
            tmp(2) match {
              case '#' => "\"^" + tmp(1) + tmp.drop(3)
              case 'b' => "\"_" + tmp(1) + tmp.drop(3)
              case _ => tmp
            }
          })

          abcRgx.replaceAllIn(y, n => n.toString match {
            // replace chord
            case z if z.matches(shortChordRgx.toString) =>
              "\"" + toNote(z.toString.tail).toString.replaceAll("\\d","")
            // replace note
            case _ => toNote(n.toString).transpose(if (up) 12 else -12).toAbc
          }).replace(breaker,"")
        }
      }
    }.mkString("\n")

    sanitize(newMusic)
  }

  @JSExport
  def octaveUp(music:String):String = transposeOctave(music, up=true);
  @JSExport
  def octaveDown(music:String):String = transposeOctave(music, up=false);

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
