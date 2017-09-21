package Music

import scala.scalajs.js.annotation._
import scala.scalajs.js
import Note.{letterNames, letterNamesLooped}

@JSExportTopLevel("util")
object AbcJsExt {
  //val pianoNotes = Vector("C", "D", "E", "F", "g", "a", "b")
  //val halfSteps = Vector(1, 2, 2, 1, 2, 2, 2)

  @JSExport
  def getKey(header:String):String = {
    "K:\\s*\\w+".r.findFirstIn(header) match {
      case Some(x) => x
      case _ => "C"
    }
  }

  def isAccidental(x:String) = List("^","_","^^","__").contains(x)
  def isOctave(x:String) = List(",","'").contains(x)
  val abcRgx = "[_^]*[ABCDEFGabcdefg][',]*".r

  def toNote(abcNote:String):Note = {
    val letter = "[ABCDEFGabcdefg]".r.findFirstIn(abcNote) match {
      case Some(x) => x
      case _ => ""
    }
    val octave = "[',]+".r.findFirstIn(abcNote) match {
      case Some(x) => 4 - x.count(_ == ',') + x.count(_ == ''')
      case _ => 4
    }
    val accidental = "[_^]+".r.findFirstIn(abcNote) match {
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
    val oldText = m("text")
    val oldKeyHeader = getKey(oldHeader)
    val oldKey = oldKeyHeader.split(":").map(_.trim).last
    val halfSteps = toNote(key) - toNote(oldKey)

    val enDist = letterNames.indexOf(toNote(key).letter) - 
                 letterNames.indexOf(toNote(oldKey).letter)

    val newText = abcRgx.replaceAllIn(oldText, n => {
      lazy val note = toNote(n.toString)//.toAbsoluteNote(oldKey)
      lazy val newNote = note.transpose(halfSteps)
      //newNote.toAbc
      val enIdx = letterNames.indexOf(note.letter) + enDist
      val lname = letterNamesLooped(enIdx)
      //print(newNote.toEnharmonic(lname) + " ")
      //print(newNote.toEnharmonic(lname).toRelativeNote(key) + ", ")
      newNote.toEnharmonic(lname).toAbc
      //what I want:
      //toNote(n.toString).transposeWithKey(oldKey, key)
    })

    val newHeader = oldHeader.replace(oldKeyHeader, s"K:$key\n") + "\n"

    sanitize(newHeader + newText)
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
  def tester() {
    println("Printed Tests:")
    println("sharpOrder: " + Note.sharpOrder.toString)
    println("flatOrder: "  + Note.flatOrder)
    println("Circle 5: "   + Note.circleOf5th)
    println("Circle 4: "   + Note.circleOf4th)
    println("KeySigs: " + Note.keySigs.toList)
  }
}

