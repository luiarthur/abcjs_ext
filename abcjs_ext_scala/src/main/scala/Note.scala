package Music

object Note {
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
}


case class Note(letter: String, octave: Int, accidental: String="") {
  import Note._

  override def toString: String = {
    letter + octave + accidental
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
