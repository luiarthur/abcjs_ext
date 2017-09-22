package Music
import scala.scalajs.js

object Note {
  def insert(s:String, pos:Int, x:String) = {
    val (a,b) = s.splitAt(pos)
    a + x + b
  }
  val letterNames = {'A' to 'G'}.map(_.toString).toVector
  val octaves = Vector.range(0,8)
  val accidentals = List("#", "b", "##", "bb", "", "n")

  val lowestNote = "A0"
  val HighestNote = "C8"
  val sharpableNotes = Vector("C", "D", "F", "G", "A")

  val pianoNotes = Vector(
      "A0", "A0#", "B0", 
      "C1", "C1#", "D1", "D1#", "E1", "F1", "F1#", "G1", "G1#", "A1", "A1#", "B1",
      "C2", "C2#", "D2", "D2#", "E2", "F2", "F2#", "G2", "G2#", "A2", "A2#", "B2",
      "C3", "C3#", "D3", "D3#", "E3", "F3", "F3#", "G3", "G3#", "A3", "A3#", "B3",
      "C4", "C4#", "D4", "D4#", "E4", "F4", "F4#", "G4", "G4#", "A4", "A4#", "B4",
      "C5", "C5#", "D5", "D5#", "E5", "F5", "F5#", "G5", "G5#", "A5", "A5#", "B5",
      "C6", "C6#", "D6", "D6#", "E6", "F6", "F6#", "G6", "G6#", "A6", "A6#", "B6",
      "C7", "C7#", "D7", "D7#", "E7", "F7", "F7#", "G7", "G7#", "A7", "A7#", "B7",
      "C8")

  def letterNamesLooped(i:Int):String = {
    val idx = i compareTo 0 match {
      case -1 => letterNames.size + i
      case _ => i % letterNames.size
    }
    letterNames(idx)
  }

  //Keys
  val firstFlat = "Bb"
  val firstSharp = "F#"

  def generateAccidentals(list:List[String], halfSteps:Int, maxSize:Int, acc:String, circleOf:Int):Vector[String] = {
    if (list.size == maxSize) list.reverse.toVector else {
      val currentNote = Note(list.head.head.toString, 4, list.head.drop(1))
      val newNoteTmp = currentNote.transpose(halfSteps)

      val enIdx = letterNames.indexOf(currentNote.letter) + circleOf - 1
      val newNoteEn = letterNamesLooped(enIdx)
      val newNote = newNoteTmp.toEnharmonic(newNoteEn)

      generateAccidentals(newNote.letter + newNote.accidental :: list, halfSteps, maxSize, acc, circleOf)
    }
  }

  val sharpOrder = generateAccidentals(List(firstSharp), halfSteps=7, maxSize=7, acc="#", circleOf=5)
  val flatOrder = generateAccidentals(List(firstFlat), halfSteps=5, maxSize=7, acc="b", circleOf=4)

  val circleOf5th = generateAccidentals(List("C"), halfSteps=7, maxSize=8,acc="#",circleOf=5)
  val circleOf4th = generateAccidentals(List("C"), halfSteps=5, maxSize=8,acc="b",circleOf=4)

  val keySigs = {
    val m = js.Dictionary("C" -> Vector[String]())
    var i = 1
    while (i < 8) {
      m.update(circleOf5th(i), sharpOrder.take(i))
      m.update(circleOf4th(i),  flatOrder.take(i))
      i += 1
    }
    m
  }
  
  
}


case class Note(letter: String, octave: Int, accidental: String="") {
  import Note._

  override def toString: String = {
    letter + octave + accidental
  }


  def toAbsoluteNote(key:String):Note = {
    val ks = keySigs(key).filter(_.contains(letter))

    (this.accidental, ks.size) match {
      case ("", 1) => Note(letter, octave, ks.head.drop(1))
      case ("n", 1) => Note(letter, octave, accidental)
      case _ => this
    }
  }

  //def toRelativeNote(key:String):Note = this
  def toRelativeNote(key:String):Note = {
    val ks = keySigs(key)

    // Check if this.Note's lettername is in the key signature
    if (ks.exists(_.head.toString == letter)) {
      val ksAcc = ks.head.drop(1)
      (accidental, ksAcc) match {
        case ("n",_) => Note(letter, octave, "n")
        case ("",_) => Note(letter, octave, "n")
        case (a,k) if a != k => Note(letter, octave, a)
        case (a,k) if a == k => Note(letter, octave, "")
        case _ => this
      }
    } else this
  }

  def standardize: Note = { // Note with sharp
    val halfSteps = accidental match {
      case "#"  =>  1
      case "##"  =>  2
      case "b"  => -1
      case "bb" => -2
      case _ =>  0 // "=" or ""
    }

    val currentIdx = pianoNotes.indexOf(letter.toString() + octave)
    val enharmonicIdx = currentIdx + halfSteps

    val note = if (currentIdx > -1 && 0 <= enharmonicIdx && enharmonicIdx < 88) {
      val enharmonic= pianoNotes(enharmonicIdx)
      val eLetter = enharmonic(0).toString
      val eOctave = enharmonic(1).toString.toInt
      val eAccidental = enharmonic.drop(2)

      Note(eLetter, eOctave, eAccidental)
    } else Note("", 0, "")

    note
  }

  def ==(that: Note): Boolean = {
    this.standardize.toString == that.standardize.toString
  }

  def toEnharmonic(e: String): Note = { // FIXME
    // return enharmonic equivalent using `letter` in spelling
    val candidates = accidentals.map{ acc => 
      val oct = (e, letter) match {
        case ("B", "C") => octave - 1
        case ("C", "B") => octave + 1
        case _ => octave
      }
      Note(e, oct, acc) 
    }
    candidates.filter{ _ == this }.head
  }

  def isValid: Boolean = this == Note("", 0, "")


  def transpose(halfSteps: Int): Note = { 
    require(-12 <= halfSteps && halfSteps <= 12,
      "halfSteps must be between -12 and 12 (inclusive)."
    )
    val idx = pianoNotes.indexOf(this.standardize.toString) + halfSteps
    val pi = pianoNotes(idx)
    Note(pi(0).toString, pi(1).toString.toInt, pi.drop(2))
  }

  def isSpecialNoteInKey(key:String):Boolean = {
    keySigs(key).exists(_.head.toString == letter)
  }

  def letterDist(that:Note):Int = {
    letterNames.indexOf(this.letter) - letterNames.indexOf(that.letter)
  }

  def transposeWithKey(oldKey:String, newKey:String): Note = { 
    val newKeyNote =  Note(oldKey.head.toString, 4, oldKey.drop(1))
    val oldKeyNote =  Note(newKey.head.toString, 4, newKey.drop(1))
    val halfSteps = newKeyNote - oldKeyNote

    val newNote = {
      val newNoteStd:Note = this.transpose(halfSteps)
      val letterD = this.letterDist(oldKeyNote)
      println(this)
      println(oldKeyNote)
      println(letterD)
      val enNote = letterNamesLooped(letterNames.indexOf(this.letter) + letterD)
      this.toEnharmonic(enNote)
    }

    newNote
    //???
  }


  def -(that: Note): Int = {
    pianoNotes.indexOf(this.standardize.toString) - 
    pianoNotes.indexOf(that.standardize.toString)
  }

  def toAbc: String = {
    val abcAcc = accidental match {
      case "#" => "^"
      case "##" => "^^"
      case "b" => "_"
      case "bb" => "__"
      case "n" => "="
      case _ => ""
    }

    val abcOct = {if (octave > 0) "'" * (octave-4) else "_" * (4-octave)}

    abcAcc + letter + abcOct
  }

}
