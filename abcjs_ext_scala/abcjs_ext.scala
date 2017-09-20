import scala.scalajs.js.annotation._
import scala.scalajs.js

@JSExportTopLevel("ABCJS_Ext")
object AbcJsExt{
  @JSExport
  val pianoNotes = 
    js.Array("a", "b", "c", "d", "e", "f", "g")


  @JSExport
  def inPianoNotes(note: String):Boolean = {
    pianoNotes.contains(note)
  }

  @JSExport
  def transpose(music:String, key:String, octave:Int= -1):String = {
    ???
  }

  @JSExport
  def getChunks(music:String):js.Array[String] = {
    val header:String = ???
    val text:String = ???
    js.Array(header, text)
  }

}

